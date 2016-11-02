package mesosphere.marathon
package api.v2

import mesosphere.marathon.raml._
import mesosphere.marathon.state.FetchUri

trait AppNormalization {

  import AppNormalization._
  import Apps._

  /**
    * Ensure backwards compatibility by adding portIndex to health checks when necessary.
    *
    * In the past, healthCheck.portIndex was required and had a default value 0. When we introduced healthCheck.port, we
    * made it optional (also with ip-per-container in mind) and we have to re-add it in cases where it makes sense.
    */
  def normalizeHealthChecks(healthChecks: Set[AppHealthCheck]): Set[AppHealthCheck] = {
    def withPort(check: AppHealthCheck): AppHealthCheck = {
      def needsDefaultPortIndex = check.port.isEmpty && check.portIndex.isEmpty
      if (needsDefaultPortIndex) check.copy(portIndex = Some(0)) else check
    }

    healthChecks.map {
      case check: AppHealthCheck if check.protocol != AppHealthCheckProtocol.Command => withPort(check)
      case check => check
    }
  }

  def normalizeFetch(maybeUris: Option[Seq[String]], maybeFetch: Option[Seq[Artifact]]): Option[Seq[Artifact]] =
    (maybeUris, maybeFetch) match {
      case (Some(uris), fetch) if uris.nonEmpty && fetch.fold(true)(_.isEmpty) =>
        Some(uris.map(uri => Artifact(uri = uri, extract = FetchUri.isExtract(uri))))
      case (Some(uris), Some(fetch)) if uris.nonEmpty && fetch.nonEmpty =>
        throw SerializationFailedException("cannot specify both uris and fetch fields")
      case _ => maybeFetch
    }

  /**
    * currently invoked prior to validation, so that we only validate portMappings once
    */
  def migrateDockerPortMappings(container: Container): Container = {
    def translatePortMappings(dockerPortMappings: Seq[ContainerPortMapping]): Option[Seq[ContainerPortMapping]] =
      (container.portMappings.isEmpty, dockerPortMappings.isEmpty) match {
        case (false, false) =>
          throw SerializationFailedException("cannot specify both portMappings and docker.portMappings")
        case (false, true) =>
          container.portMappings
        case (true, false) =>
          Option(dockerPortMappings)
        case _ =>
          None
      }

    container.docker.map(_.portMappings) match {
      case Some(portMappings) => container.copy(
        portMappings = translatePortMappings(portMappings),
        docker = container.docker.map(_.copy(portMappings = Nil))
      )
      case None => container
    }

    // note, we leave container.docker.network alone because we'll need that for app normalization
  }

  def migrateIpDiscovery(container: Container, maybeDiscovery: Option[IpDiscovery]): Container =
    // assume that migrateDockerPortMappings has already happened and so container.portMappings is now the
    // source of truth for any port-mappings specified at the container level.
    (container.`type`, maybeDiscovery) match {
      case (EngineType.Mesos, Some(discovery)) if discovery.ports.nonEmpty =>
        if (container.portMappings.nonEmpty)
          throw SerializationFailedException("container.portMappings and ipAddress.discovery.ports must not both be set")
        val portMappings = discovery.ports.map { port =>
          ContainerPortMapping(
            containerPort = port.number,
            hostPort = None, // the old IP/CT api didn't let users map container ports to host ports
            name = Some(port.name),
            protocol = port.protocol
          )
        }
        container.copy(portMappings = Option(portMappings))
      case (t, Some(discovery)) if discovery.ports.nonEmpty =>
        throw SerializationFailedException(s"ipAddress.discovery.ports do not apply for container type $t")
      case _ =>
        container
    }

  def normalizePortMappings(networks: Option[Seq[Network]], container: Option[Container]): Option[Container] = {
    // assuming that we're already validated and everything ELSE network-related has been normalized, we can now
    // deal with translating unspecified port-mapping host-port's when in bridge mode
    val isBridgedNetwork = networks.fold(false)(_.exists(_.mode == NetworkMode.ContainerBridge))
    container.map { ct =>
      ct.copy(
        docker = ct.docker.map { d =>
          // this is deprecated, clear it so that it's deterministic later on...
          d.copy(network = None)
        },
        portMappings =
          if (!isBridgedNetwork) ct.portMappings
          else ct.portMappings.map {
            _.map {
              // backwards compat: when in BRIDGE mode, missing host ports default to zero
              case m: ContainerPortMapping if m.hostPort.isEmpty =>
                m.copy(hostPort = Option(state.Container.PortMapping.HostPortDefault))
              case m => m
            }
          }
      )
    }
  }

  /**
    * only deprecated fields and their interaction with canonical fields have been validated so far,
    * so we limit normalization here to translating from the deprecated API to the canonical one.
    *
    * @return an API object in canonical form (read: doesn't use deprecated APIs)
    */
  def forDeprecatedFields(update: AppUpdate): AppUpdate = {
    val fetch = normalizeFetch(update.uris, update.fetch)

    val networks = NetworkTranslation.toNetworks(NetworkTranslation(
      update.ipAddress,
      update.container.flatMap(_.docker.flatMap(_.network)),
      update.networks
    ))

    // no container specified in JSON but ipAddress is ==> implies empty Mesos container
    val container = update.container.orElse(update.ipAddress.map(_ => Container(EngineType.Mesos))).map { c =>
      // we explicitly make the decision here to not try implementing default port-mappings.
      // after applying the app-update to an app, the system should normalize the app definition -- and at that
      // point we'll calculate defaults if needed.
      dropDockerNetworks(
        migrateIpDiscovery(
          migrateDockerPortMappings(c),
          update.ipAddress.flatMap(_.discovery)
        )
      )
    }

    // no default port calculations, as per the port-mappings comment above.
    val portDefinitions = update.portDefinitions.orElse(
      update.ports.map(_.map(port => PortDefinition(port))))

    update.copy(
      // normalize fetch
      fetch = fetch,
      uris = None,
      // normalize networks
      networks = networks,
      ipAddress = None,
      container = container,
      // ports
      portDefinitions = portDefinitions,
      ports = None,
      // health checks
      healthChecks = update.healthChecks.map(normalizeHealthChecks),
      readinessChecks = update.readinessChecks.map(_.map(normalizeReadinessCheck))
    )
  }

  def apply(update: AppUpdate, config: Config): AppUpdate = {
    val networks = config.defaultNetworkName.map { _ =>
      update.networks.map(_.map {
        case n: Network if n.name.isEmpty && n.mode == NetworkMode.Container => n.copy(name = config.defaultNetworkName)
        case n => n
      })
    }.getOrElse(update.networks)

    val container = normalizePortMappings(update.networks, update.container)
    update.copy(
      container = container,
      networks = networks
    )
  }

  def dropDockerNetworks(c: Container): Container =
    c.docker.find(_.network.nonEmpty).fold(c)(d => c.copy(docker = Some(d.copy(network = None))))

  def normalizeReadinessCheck(check: ReadinessCheck): ReadinessCheck =
    if (check.httpStatusCodesForReady.nonEmpty) check
    else check.copy(httpStatusCodesForReady = core.readiness.ReadinessCheck.DefaultHttpStatusCodesForReady)

  def maybeDropPortMappings(c: Container, networks: Seq[Network]): Container =
    // empty networks Seq defaults to host-mode later on, so consider it now as indicating host-mode networking
    if (networks.exists(_.mode == NetworkMode.Host) || networks.isEmpty) c.copy(portMappings = None) else c

  def applyDefaultPortMappings(c: Container, networks: Seq[Network]): Container =
    if (networks.exists(_.mode == NetworkMode.Host) || networks.isEmpty || c.portMappings.nonEmpty) c
    else c.copy(portMappings = Option(Seq(ContainerPortMapping(name = Option("default")))))

  def applyDefaultPortDefinitions(app: App, networks: Seq[Network]): Option[Seq[PortDefinition]] =
    // Normally, our default is one port. If an non-host networks are defined that would lead to an error
    // if left unchanged.
    if (networks.exists(_.mode != NetworkMode.Host))
      None
    else
      Some(app.portDefinitions.getOrElse(
        app.ports.map(p => PortDefinitions(p: _*)).getOrElse(
          DefaultPortDefinitions
        )
      ))

  /**
    * only deprecated fields and their interaction with canonical fields have been validated so far,
    * so we limit normalization here to translating from the deprecated API to the canonical one.
    *
    * @return an API object in canonical form (read: doesn't use deprecated APIs)
    */
  def forDeprecatedFields(app: App): App = {
    import state.PathId._
    val fetch: Seq[Artifact] = normalizeFetch(app.uris, Option(app.fetch)).getOrElse(Nil)

    val networks: Seq[Network] = NetworkTranslation.toNetworks(NetworkTranslation(
      app.ipAddress,
      app.container.flatMap(_.docker.flatMap(_.network)),
      if (app.networks.isEmpty) None else Some(app.networks)
    )).getOrElse(Nil)

    // canonical validation doesn't allow both portDefinitions and container.portMappings:
    // container and portDefinitions normalization (below) deal with dropping unsupported port configs.

    val container = app.container.orElse(
      // no container specified in JSON but ipAddress is ==> implies empty Mesos container
      app.ipAddress.map(_ => Container(EngineType.Mesos))
    ).map { c =>
        applyDefaultPortMappings(
          maybeDropPortMappings(
            dropDockerNetworks(
              migrateIpDiscovery(
                migrateDockerPortMappings(c),
                app.ipAddress.flatMap(_.discovery)
              )
            ),
            networks
          ),
          networks
        )
      }

    val portDefinitions = applyDefaultPortDefinitions(app, networks)

    val healthChecks =
      // for an app (not an update) only normalize if there are ports defined somewhere.
      // ??? intentionally consider the non-normalized portDefinitions since that's what the old Formats code did
      if (app.portDefinitions.exists(_.nonEmpty) || container.exists(_.portMappings.nonEmpty)) normalizeHealthChecks(app.healthChecks)
      else app.healthChecks

    // cheating: we know that this is invoked before canonical validation so we provide a default here.
    // it would be nice to use RAML "object" default values here but our generator isn't that smart yet.
    val residency: Option[AppResidency] = app.container.find(_.volumes.exists(_.persistent.nonEmpty))
      .fold(app.residency)(_ => app.residency.orElse(DefaultAppResidency))

    app.copy(
      // it's kind of cheating to do this here, but its required in order to pass canonical validation (that happens
      // before canonical normalization)
      id = app.id.toRootPath.toString,
      // normalize fetch
      fetch = fetch,
      uris = None,
      // normalize networks
      networks = networks,
      ipAddress = None,
      container = container,
      // normalize ports
      portDefinitions = portDefinitions,
      ports = None,
      // and the rest (simple)
      healthChecks = healthChecks,
      residency = residency,
      readinessChecks = app.readinessChecks.map(normalizeReadinessCheck)
    )
  }

  def apply(app: App, config: Config): App = {
    val networks = config.defaultNetworkName.map { _ =>
      app.networks.map {
        case n: Network if n.name.isEmpty && n.mode == NetworkMode.Container => n.copy(name = config.defaultNetworkName)
        case n => n
      }
    }.orElse(Some(app.networks)).filter(_.nonEmpty).getOrElse(DefaultNetworks)

    val container = normalizePortMappings(Some(networks), app.container)

    val defaultUnreachable: UnreachableStrategy = {
      val hasPersistentVols = app.container.exists(_.volumes.exists(_.persistent.nonEmpty))
      state.UnreachableStrategy.default(hasPersistentVols).toRaml
    }

    app.copy(
      container = container,
      networks = networks,
      unreachableStrategy = app.unreachableStrategy.orElse(Option(defaultUnreachable))
    )
  }
}

object AppNormalization extends AppNormalization {

  trait Config {
    def defaultNetworkName: Option[String]
  }

  case class Configure(override val defaultNetworkName: Option[String]) extends Config

  /**
    * attempt to translate an older app API (that uses ipAddress and container.docker.network) to the new API
    * (that uses app.networks, and container.portMappings)
    */
  case class NetworkTranslation(
    ipAddress: Option[IpAddress],
    networkType: Option[DockerNetwork],
    networks: Option[Seq[Network]])

  object NetworkTranslation {
    def toNetworks(nt: NetworkTranslation): Option[Seq[Network]] = nt match {
      case NetworkTranslation(Some(ipAddress), Some(networkType), None) =>
        // wants ip/ct with a specific network mode
        import DockerNetwork._
        networkType match {
          case Host =>
            Some(Seq(Network(mode = NetworkMode.Host))) // strange way to ask for this, but we'll accommodate
          case User =>
            Some(Seq(Network(mode = NetworkMode.Container, name = ipAddress.networkName, labels = ipAddress.labels)))
          case Bridge =>
            Some(Seq(Network(mode = NetworkMode.ContainerBridge, labels = ipAddress.labels)))
          case unsupported =>
            throw SerializationFailedException(s"unsupported docker network type $unsupported")
        }
      case NetworkTranslation(Some(ipAddress), None, None) =>
        // wants ip/ct with some network mode.
        // if the user gave us a name try to figure out what they want.
        ipAddress.networkName match {
          case Some(name) if name == Networks.DefaultMesosBridgeName => // users shouldn't do this, but we're tolerant
            Some(Seq(Network(mode = NetworkMode.ContainerBridge, labels = ipAddress.labels)))
          case name =>
            Some(Seq(Network(mode = NetworkMode.Container, name = name, labels = ipAddress.labels)))
        }
      case NetworkTranslation(None, Some(networkType), None) =>
        // user didn't ask for IP-per-CT, but specified a network type anyway
        import DockerNetwork._
        networkType match {
          case Host => Some(Seq(Network(mode = NetworkMode.Host)))
          case User => Some(Seq(Network(mode = NetworkMode.Container)))
          case Bridge => Some(Seq(Network(mode = NetworkMode.ContainerBridge)))
          case unsupported =>
            throw SerializationFailedException(s"unsupported docker network type $unsupported")
        }
      case NetworkTranslation(None, None, networks) =>
        // no deprecated APIs used! awesome, so use the canonical networks field
        networks
      case _ =>
        throw SerializationFailedException("cannot mix deprecated and canonical network APIs")
    }
  }
}
