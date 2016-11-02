package mesosphere.marathon
package raml

import mesosphere.marathon.state.{ AppDefinition, PathId, Timestamp, Group => CoreGroup, VersionInfo => CoreVersionInfo }

trait GroupConversion {

  // TODO needs a dedicated/focused unit test; other (larger) unit tests provide indirect coverage
  implicit val groupUpdateRamlReads: Reads[(UpdateGroupStructureOp, App => AppDefinition), CoreGroup] =
    Reads[(UpdateGroupStructureOp, App => AppDefinition), CoreGroup] {
      case (op, cf) =>
        op.apply(cf)
    }

  implicit val groupWritesRaml: Writes[CoreGroup, Group] =
    Writes[CoreGroup, Group] { group =>
      Group(
        id = group.id.toString,
        apps = group.apps.map { case (_, app) => Raml.toRaml(app) }(collection.breakOut),
        pods = group.pods.map { case (_, pod) => Raml.toRaml(pod) }(collection.breakOut),
        groups = group.groupsById.map { case (_, g) => Raml.toRaml(g) }(collection.breakOut),
        dependencies = group.dependencies.map(_.toString),
        version = Some(group.version.toOffsetDateTime)
      )
    }
}

object GroupConversion extends GroupConversion {
  def apply(groupUpdate: GroupUpdate, current: CoreGroup, timestamp: Timestamp): UpdateGroupStructureOp =
    UpdateGroupStructureOp(groupUpdate, current, timestamp)
}

case class UpdateGroupStructureOp(
    groupUpdate: GroupUpdate,
    current: CoreGroup,
    timestamp: Timestamp
) {
  import UpdateGroupStructureOp._

  require(groupUpdate.scaleBy.isEmpty, "For a structural update, no scale should be given.")
  require(groupUpdate.version.isEmpty, "For a structural update, no version should be given.")

  private def update(gu: GroupUpdate, g: CoreGroup, t: Timestamp): UpdateGroupStructureOp = UpdateGroupStructureOp(gu, g, t)

  def apply(implicit cf: App => AppDefinition): CoreGroup = {
    val effectiveGroups: Map[PathId, CoreGroup] = groupUpdate.groups.fold(current.groupsById) { updates =>
      updates.map { groupUpdate =>
        val gid = groupId(groupUpdate).canonicalPath(current.id)
        val newGroup = current.groupsById.get(gid).fold(toGroup(groupUpdate, gid, timestamp))(group => update(groupUpdate, group, timestamp).apply)
        newGroup.id -> newGroup
      }(collection.breakOut)
    }
    val effectiveApps: Map[AppDefinition.AppKey, AppDefinition] =
      groupUpdate.apps.map(_.map(cf)).getOrElse(current.apps.values).map { currentApp =>
        val app = toApp(current.id, currentApp, timestamp)
        app.id -> app
      }(collection.breakOut)

    val effectiveDependencies = groupUpdate.dependencies.fold(current.dependencies)(_.map(PathId(_).canonicalPath(current.id)))
    CoreGroup(
      id = current.id,
      apps = effectiveApps,
      pods = current.pods,
      groupsById = effectiveGroups,
      dependencies = effectiveDependencies,
      version = timestamp,
      transitiveAppsById = effectiveApps ++ effectiveGroups.values.flatMap(_.transitiveAppsById),
      transitivePodsById = current.pods ++ effectiveGroups.values.flatMap(_.transitivePodsById))
  }
}

object UpdateGroupStructureOp {

  def groupId(groupUpdate: GroupUpdate): PathId = groupUpdate.id.map(PathId(_)).getOrElse(
    // validation should catch this..
    throw SerializationFailedException("No group id was given!")
  )

  def toApp(gid: PathId, app: AppDefinition, version: Timestamp): AppDefinition = {
    val appId = app.id.canonicalPath(gid)
    app.copy(id = appId, dependencies = app.dependencies.map(_.canonicalPath(gid)),
      versionInfo = CoreVersionInfo.OnlyVersion(version))
  }

  def toGroup(groupUpdate: GroupUpdate, gid: PathId, version: Timestamp)(implicit cf: App => AppDefinition): CoreGroup = {
    val appsById: Map[AppDefinition.AppKey, AppDefinition] = groupUpdate.apps.getOrElse(Set.empty).map { currentApp =>
      val app = toApp(gid, cf(currentApp), version)
      app.id -> app
    }(collection.breakOut)

    val groupsById: Map[PathId, CoreGroup] = groupUpdate.groups.getOrElse(Seq.empty).map { currentGroup =>
      val group = toGroup(currentGroup, groupId(currentGroup).canonicalPath(gid), version)
      group.id -> group
    }(collection.breakOut)

    CoreGroup(
      id = gid,
      apps = appsById,
      pods = Map.empty,
      groupsById = groupsById,
      dependencies = groupUpdate.dependencies.fold(Set.empty[PathId])(_.map(PathId(_).canonicalPath(gid))),
      version = version,
      transitiveAppsById = appsById ++ groupsById.values.flatMap(_.transitiveAppsById),
      transitivePodsById = Map.empty // we don't support updates to pods via group-update operations
    )
  }
}

