package mesosphere.marathon
package api.v2

import java.net.URI
import javax.inject.Inject
import javax.servlet.http.HttpServletRequest
import javax.ws.rs._
import javax.ws.rs.core.{ Context, MediaType, Response }

import akka.event.EventStream
import mesosphere.marathon.api.v2.Validation._
import mesosphere.marathon.api.v2.validation.AppValidation
import mesosphere.marathon.api.v2.json.Formats._
import mesosphere.marathon.api.{ AuthResource, MarathonMediaType, RestResource }
import mesosphere.marathon.core.appinfo.{ AppInfo, AppInfoService, AppSelector, Selector, TaskCounts }
import mesosphere.marathon.core.base.Clock
import mesosphere.marathon.core.event.ApiPostEvent
import mesosphere.marathon.core.group.GroupManager
import mesosphere.marathon.core.plugin.PluginManager
import mesosphere.marathon.plugin.auth._
import mesosphere.marathon.raml.{ AppConversion, Raml }
import mesosphere.marathon.state.PathId._
import mesosphere.marathon.state._
import mesosphere.marathon.stream.Implicits._
import play.api.libs.json.Json

@Path("v2/apps")
@Consumes(Array(MediaType.APPLICATION_JSON))
@Produces(Array(MarathonMediaType.PREFERRED_APPLICATION_JSON))
class AppsResource @Inject() (
    clock: Clock,
    eventBus: EventStream,
    appTasksRes: AppTasksResource,
    service: MarathonSchedulerService,
    appInfoService: AppInfoService,
    val config: MarathonConf,
    groupManager: GroupManager,
    pluginManager: PluginManager)(implicit
  val authenticator: Authenticator,
    val authorizer: Authorizer) extends RestResource with AuthResource {

  import AppsResource._
  import Normalization._

  private[this] val ListApps = """^((?:.+/)|)\*$""".r
  private implicit lazy val appDefinitionValidator = AppDefinition.validAppDefinition(config.availableFeatures)(pluginManager)
  private implicit lazy val validateCanonicalAppUpdateAPI = AppValidation.validateCanonicalAppUpdateAPI(config.availableFeatures)

  private val normalizationConfig = AppNormalization.Configure(config.defaultNetworkName.get)

  private implicit val validateAndNormalizeApp: Normalization[raml.App] =
    appNormalization(NormalizationConfig(config.availableFeatures, normalizationConfig))

  private implicit val validateAndNormalizeAppUpdate: Normalization[raml.AppUpdate] =
    appUpdateNormalization(NormalizationConfig(config.availableFeatures, normalizationConfig))

  @GET
  def index(
    @QueryParam("cmd") cmd: String,
    @QueryParam("id") id: String,
    @QueryParam("label") label: String,
    @QueryParam("embed") embed: java.util.Set[String],
    @Context req: HttpServletRequest): Response = authenticated(req) { implicit identity =>
    val selector = selectAuthorized(search(Option(cmd), Option(id), Option(label)))
    // additional embeds are deprecated!
    val resolvedEmbed = InfoEmbedResolver.resolveApp(embed) +
      AppInfo.Embed.Counts + AppInfo.Embed.Deployments
    val mapped = result(appInfoService.selectAppsBy(selector, resolvedEmbed))
    Response.ok(jsonObjString("apps" -> mapped)).build()
  }

  @POST
  def create(
    body: Array[Byte],
    @DefaultValue("false")@QueryParam("force") force: Boolean,
    @Context req: HttpServletRequest): Response = authenticated(req) { implicit identity =>

    assumeValid {
      val rawApp = Raml.fromRaml(Json.parse(body).as[raml.App].norm)
      val now = clock.now()
      val app = validateOrThrow(rawApp.withCanonizedIds()).copy(versionInfo = VersionInfo.OnlyVersion(now))

      checkAuthorization(CreateRunSpec, app)

      def createOrThrow(opt: Option[AppDefinition]) = opt
        .map(_ => throw ConflictingChangeException(s"An app with id [${app.id}] already exists."))
        .getOrElse(app)

      val plan = result(groupManager.updateApp(app.id, createOrThrow, app.version, force))

      val appWithDeployments = AppInfo(
        app,
        maybeCounts = Some(TaskCounts.zero),
        maybeTasks = Some(Seq.empty),
        maybeDeployments = Some(Seq(Identifiable(plan.id)))
      )

      maybePostEvent(req, appWithDeployments.app)
      Response
        .created(new URI(app.id.toString))
        .header(RestResource.DeploymentHeader, plan.id)
        .entity(jsonString(appWithDeployments))
        .build()
    }
  }

  @GET
  @Path("""{id:.+}""")
  def show(
    @PathParam("id") id: String,
    @QueryParam("embed") embed: java.util.Set[String],
    @Context req: HttpServletRequest): Response = authenticated(req) { implicit identity =>
    val resolvedEmbed = InfoEmbedResolver.resolveApp(embed) ++ Set(
      // deprecated. For compatibility.
      AppInfo.Embed.Counts, AppInfo.Embed.Tasks, AppInfo.Embed.LastTaskFailure, AppInfo.Embed.Deployments
    )

    def transitiveApps(groupId: PathId): Response = {
      groupManager.group(groupId) match {
        case Some(group) =>
          checkAuthorization(ViewGroup, group)
          val appsWithTasks = result(appInfoService.selectAppsInGroup(groupId, authzSelector, resolvedEmbed))
          ok(jsonObjString("*" -> appsWithTasks))
        case None =>
          unknownGroup(groupId)
      }
    }

    def app(appId: PathId): Response = {
      result(appInfoService.selectApp(appId, authzSelector, resolvedEmbed)) match {
        case Some(appInfo) =>
          checkAuthorization(ViewRunSpec, appInfo.app)
          ok(jsonObjString("app" -> appInfo))
        case None => unknownApp(appId)
      }
    }

    id match {
      case ListApps(gid) => transitiveApps(gid.toRootPath)
      case _ => app(id.toRootPath)
    }
  }

  @PUT
  @Path("""{id:.+}""")
  def replace(
    @PathParam("id") id: String,
    body: Array[Byte],
    @DefaultValue("false")@QueryParam("force") force: Boolean,
    @Context req: HttpServletRequest): Response = authenticated(req) { implicit identity =>
    val appId = id.toRootPath
    val now = clock.now()

    assumeValid {
      val appUpdate: raml.AppUpdate = Json.parse(body).as[raml.AppUpdate].copy(id = Some(appId.toString)).norm
      val plan = result(groupManager.updateApp(appId, updateOrCreate(appId, _, appUpdate), now, force))

      val response = plan.original.app(appId)
        .map(_ => Response.ok())
        .getOrElse(Response.created(new URI(appId.toString)))
      plan.target.app(appId).foreach { appDef =>
        maybePostEvent(req, appDef)
      }
      deploymentResult(plan, response)
    }
  }

  @PUT
  def replaceMultiple(
    @DefaultValue("false")@QueryParam("force") force: Boolean,
    body: Array[Byte],
    @Context req: HttpServletRequest): Response = authenticated(req) { implicit identity =>

    assumeValid {
      val updates = validateOrThrow(Json.parse(body).as[Seq[raml.AppUpdate]].map { upd =>
        withCanonizedIds(upd.norm)
      })

      val version = clock.now()

      def updateGroup(rootGroup: RootGroup): RootGroup = updates.foldLeft(rootGroup) { (group, update) =>
        update.id.map(PathId(_)) match {
          case Some(id) => group.updateApp(id, updateOrCreate(id, _, update), version)
          case None => group
        }
      }

      deploymentResult(result(groupManager.updateRoot(PathId.empty, updateGroup, version, force)))
    }
  }

  @DELETE
  @Path("""{id:.+}""")
  def delete(
    @DefaultValue("true")@QueryParam("force") force: Boolean,
    @PathParam("id") id: String,
    @Context req: HttpServletRequest): Response = authenticated(req) { implicit identity =>
    val appId = id.toRootPath

    def deleteApp(rootGroup: RootGroup) = {
      checkAuthorization(DeleteRunSpec, rootGroup.app(appId), AppNotFoundException(appId))
      rootGroup.removeApp(appId)
    }

    deploymentResult(result(groupManager.updateRoot(appId.parent, deleteApp, force = force)))
  }

  @Path("{appId:.+}/tasks")
  def appTasksResource(): AppTasksResource = appTasksRes

  @Path("{appId:.+}/versions")
  def appVersionsResource(): AppVersionsResource = new AppVersionsResource(service, groupManager, authenticator,
    authorizer, config)

  @POST
  @Path("{id:.+}/restart")
  def restart(
    @PathParam("id") id: String,
    @DefaultValue("false")@QueryParam("force") force: Boolean,
    @Context req: HttpServletRequest): Response = authenticated(req) { implicit identity =>
    val appId = id.toRootPath

    def markForRestartingOrThrow(opt: Option[AppDefinition]) = {
      opt
        .map(checkAuthorization(UpdateRunSpec, _))
        .map(_.markedForRestarting)
        .getOrElse(throw AppNotFoundException(appId))
    }

    val newVersion = clock.now()
    val restartDeployment = result(
      groupManager.updateApp(id.toRootPath, markForRestartingOrThrow, newVersion, force)
    )

    deploymentResult(restartDeployment)
  }

  private[v2] def updateOrCreate(
    appId: PathId,
    existing: Option[AppDefinition],
    appUpdate: raml.AppUpdate)(implicit identity: Identity): AppDefinition = {
    def createApp(): AppDefinition = {
      val app = withoutPriorAppDefinition(appUpdate, appId).norm
      // versionInfo doesn't change - it's never overridden by an AppUpdate.
      // the call to fromRaml loses the original versionInfo; it's just the current time in this case
      // so we just query for that (using a more predictable clock than AppDefinition has access to)
      val appDef = validateOrThrow(Raml.fromRaml(app).copy(versionInfo = VersionInfo.OnlyVersion(clock.now())))
      checkAuthorization(CreateRunSpec, appDef)
    }

    def updateApp(current: AppDefinition): AppDefinition = {
      val app = Raml.fromRaml(appUpdate -> current).norm
      // versionInfo doesn't change - it's never overridden by an AppUpdate.
      // the call to fromRaml loses the original versionInfo; we take special care to preserve it
      val appDef = validateOrThrow(Raml.fromRaml(app).copy(versionInfo = current.versionInfo))
      checkAuthorization(UpdateRunSpec, appDef)
    }

    def rollback(current: AppDefinition, version: Timestamp): AppDefinition = {
      val app = service.getApp(appId, version).getOrElse(throw AppNotFoundException(appId))
      checkAuthorization(ViewRunSpec, app)
      checkAuthorization(UpdateRunSpec, current)
      app
    }

    def updateOrRollback(current: AppDefinition): AppDefinition = appUpdate.version
      .map(v => rollback(current, Timestamp(v)))
      .getOrElse(updateApp(current))

    existing match {
      case Some(app) =>
        // we can only rollback existing apps because we deleted all old versions when dropping an app
        updateOrRollback(app)
      case None =>
        createApp()
    }
  }

  private def maybePostEvent(req: HttpServletRequest, app: AppDefinition) =
    eventBus.publish(ApiPostEvent(req.getRemoteAddr, req.getRequestURI, app))

  private[v2] def search(cmd: Option[String], id: Option[String], label: Option[String]): AppSelector = {
    def containCaseInsensitive(a: String, b: String): Boolean = b.toLowerCase contains a.toLowerCase
    val selectors = Seq[Option[Selector[AppDefinition]]](
      cmd.map(c => Selector(_.cmd.exists(containCaseInsensitive(c, _)))),
      id.map(s => Selector(app => containCaseInsensitive(s, app.id.toString))),
      label.map(new LabelSelectorParsers().parsed)
    ).flatten
    Selector.forall(selectors)
  }

  def selectAuthorized(fn: => AppSelector)(implicit identity: Identity): AppSelector = {
    Selector.forall(Seq(authzSelector, fn))
  }
}

object AppsResource {

  case class NormalizationConfig(enabledFeatures: Set[String], config: AppNormalization.Config)

  def appNormalization(config: NormalizationConfig): Normalization[raml.App] = Normalization { app =>
    validateOrThrow(app)(AppValidation.validateOldAppAPI)
    val migrated = AppNormalization.forDeprecatedFields(app)
    validateOrThrow(migrated)(AppValidation.validateCanonicalAppAPI(config.enabledFeatures))
    AppNormalization(migrated, config.config)
  }

  def appUpdateNormalization(config: NormalizationConfig): Normalization[raml.AppUpdate] = Normalization { app =>
    validateOrThrow(app)(AppValidation.validateOldAppUpdateAPI)
    val migrated = AppNormalization.forDeprecatedFields(app)
    validateOrThrow(app)(AppValidation.validateCanonicalAppUpdateAPI(config.enabledFeatures))
    AppNormalization(migrated, config.config)
  }

  def authzSelector(implicit authz: Authorizer, identity: Identity): AppSelector = Selector[AppDefinition] { app =>
    authz.isAuthorized(identity, ViewRunSpec, app)
  }

  /**
    * Create an App from an AppUpdate. This basically applies when someone abuses our API to create apps
    * using the `PUT` method: an AppUpdate is submitted for an App that doesn't actually exist. Legacy behavior
    * is to swallow our pride and convert the "update" operation into a "create" operation. This helper func
    * facilitates that.
    */
  def withoutPriorAppDefinition(update: raml.AppUpdate, appId: PathId): raml.App = {
    val selectedStrategy = AppConversion.ResidencyAndUpgradeStrategy(
      residency = update.residency.map(Raml.fromRaml(_)),
      upgradeStrategy = update.upgradeStrategy.map(Raml.fromRaml(_)),
      hasPersistentVolumes = update.container.exists(_.volumes.exists(_.persistent.nonEmpty)),
      hasExternalVolumes = update.container.exists(_.volumes.exists(_.external.nonEmpty))
    )
    val template = AppDefinition(
      appId, residency = selectedStrategy.residency, upgradeStrategy = selectedStrategy.upgradeStrategy)
    Raml.fromRaml(update -> template)
  }

  def withCanonizedIds(update: raml.AppUpdate, base: PathId = PathId.empty): raml.AppUpdate = update.copy(
    id = update.id.map(id => PathId(id).canonicalPath(base).toString),
    dependencies = update.dependencies.map(_.map(dep => PathId(dep).canonicalPath(base).toString))
  )
}
