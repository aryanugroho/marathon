package mesosphere.marathon
package api.v2

import mesosphere.UnitTest
import mesosphere.marathon.raml._

class AppNormalizationTest extends UnitTest {

  import Normalization._

  "AppNormalization" should {

    "normalize readiness checks" when {
      "readiness check does not specify status codes for ready" in {
        val check = ReadinessCheck()
        val normalized = AppNormalization.normalizeReadinessCheck(check)
        normalized should be(check.copy(httpStatusCodesForReady = Set(200)))
      }
      "readiness check does specify status codes for ready" in {
        val check = ReadinessCheck(httpStatusCodesForReady = Set(203, 204, 205, 206))
        val normalized = AppNormalization.normalizeReadinessCheck(check)
        normalized should be(check)
      }
    }

    "normalize health checks" when {

      def networkBasedHealthCheck(check: AppHealthCheck): Unit = {
        s"${check.protocol} health check does not contain port or port index" in {
          check.port should be('empty)
          check.portIndex should be('empty)

          val normalized = AppNormalization.normalizeHealthChecks(Set(check))
          normalized should be(Set(check.copy(portIndex = Option(0))))
        }
        s"${check.protocol} health check w/ port spec isn't normalized" in {
          val checkWithPort = check.copy(port = Option(88))
          checkWithPort.portIndex should be('empty)

          val normalized = AppNormalization.normalizeHealthChecks(Set(checkWithPort))
          normalized should be(Set(checkWithPort))
        }
        s"${check.protocol} health check w/ port index spec isn't normalized" in {
          val checkWithPort = check.copy(portIndex = Option(5))
          checkWithPort.port should be('empty)

          val normalized = AppNormalization.normalizeHealthChecks(Set(checkWithPort))
          normalized should be(Set(checkWithPort))
        }
      }

      behave like networkBasedHealthCheck(AppHealthCheck(protocol = AppHealthCheckProtocol.Http))
      behave like networkBasedHealthCheck(AppHealthCheck(protocol = AppHealthCheckProtocol.Https))
      behave like networkBasedHealthCheck(AppHealthCheck(protocol = AppHealthCheckProtocol.Tcp))
      behave like networkBasedHealthCheck(AppHealthCheck(protocol = AppHealthCheckProtocol.MesosHttp))
      behave like networkBasedHealthCheck(AppHealthCheck(protocol = AppHealthCheckProtocol.MesosHttps))
      behave like networkBasedHealthCheck(AppHealthCheck(protocol = AppHealthCheckProtocol.MesosTcp))

      "COMMAND health check isn't changed" in {
        val check = AppHealthCheck(protocol = AppHealthCheckProtocol.Command)
        val normalized = AppNormalization.normalizeHealthChecks(Set(check))
        normalized should be(Set(check))
      }
    }

    "normalize fetch and uris fields" when {
      "uris are present and fetch is not" in {
        val urisNoFetch = AppNormalization.normalizeFetch(Option(Seq("a")), None)
        val expected = Option(Seq(Artifact("a", extract = false)))
        urisNoFetch should be(expected)
      }
      "uris are present and fetch is an empty list" in {
        val urisEmptyFetch = AppNormalization.normalizeFetch(Option(Seq("a")), Option(Nil))
        val expected = Option(Seq(Artifact("a", extract = false)))
        urisEmptyFetch should be(expected)
      }
      "fetch is present and uris are not" in {
        val fetchNoUris = AppNormalization.normalizeFetch(None, Option(Seq(Artifact("a"))))
        val expected = Option(Seq(Artifact("a")))
        fetchNoUris should be(expected)
      }
      "fetch is present and uris are an empty list" in {
        val fetchEmptyUris = AppNormalization.normalizeFetch(Option(Nil), Option(Seq(Artifact("a"))))
        val expected = Option(Seq(Artifact("a")))
        fetchEmptyUris should be(expected)
      }
    }

    "migrate ipAddress discovery to container port mappings with a default network specified" when {
      val defaultNetworkName = Option("default-network0")
      implicit val appNormalizer = Normalization[App] { app =>
        AppNormalization(AppNormalization.forDeprecatedFields(app), AppNormalization.Configure(defaultNetworkName))
      }

      "using legacy docker networking API, without a named network" in new Fixture {
        val normalized = legacyDockerApp.copy(ipAddress = Option(IpAddress())).norm
        normalized should be(normalizedDockerApp.copy(networks = Seq(Network(name = defaultNetworkName))))
      }

      "using legacy IP/CT networking API without a named network" in new Fixture {
        legacyMesosApp.copy(ipAddress = legacyMesosApp.ipAddress.map(_.copy(
          networkName = None))).norm should be(normalizedMesosApp.copy(networks = Seq(Network(name = defaultNetworkName))))
      }
    }

    "migrate ipAddress discovery to container port mappings without a default network specified" when {
      implicit val appNormalizer = Normalization[App] { app =>
        AppNormalization(AppNormalization.forDeprecatedFields(app), AppNormalization.Configure(None))
      }

      "using legacy docker networking API" in new Fixture {
        val normalized = legacyDockerApp.norm
        normalized should be(normalizedDockerApp)
      }

      "using legacy docker networking API, without a named network" in new Fixture {
        val normalized = legacyDockerApp.copy(ipAddress = Option(IpAddress())).norm
        normalized should be(normalizedDockerApp.copy(networks = Seq(Network())))
      }

      "using legacy docker networking API w/ extraneous ipAddress discovery ports" in new Fixture {
        val ex = intercept[SerializationFailedException] {
          legacyDockerApp.copy(ipAddress = legacyDockerApp.ipAddress.map(_.copy(discovery =
            Option(IpDiscovery(
              ports = Seq(IpDiscoveryPort(34, "port1"))
            ))
          ))).norm
        }
        ex.getMessage should include("discovery.ports")
      }

      "using legacy IP/CT networking API" in new Fixture {
        legacyMesosApp.norm should be(normalizedMesosApp)
      }

      "using legacy IP/CT networking API without a named network" in new Fixture {
        legacyMesosApp.copy(ipAddress = legacyMesosApp.ipAddress.map(_.copy(
          networkName = None))).norm should be(normalizedMesosApp.copy(networks = Seq(Network())))
      }
    }

    "not assign defaults for app update normalization" when {
      implicit val appUpdateNormalizer = Normalization[AppUpdate] { app =>
        AppNormalization(AppNormalization.forDeprecatedFields(app), AppNormalization.Configure(None))
      }

      "for an empty app update" in {
        val raw = AppUpdate()
        raw.norm should be(raw)
      }

      "for an empty docker app update" in {
        val raw = AppUpdate(
          container = Option(Container(
            `type` = EngineType.Docker,
            docker = Option(DockerContainer(
              image = "image0"
            ))
          )),
          networks = Option(Seq(Network()))
        )
        raw.norm should be(raw)
      }
    }

    "preserve user intent w/ respect to opting into and out of default ports" when {

      implicit val appNormalizer = Normalization[App] { app =>
        AppNormalization(AppNormalization.forDeprecatedFields(app), AppNormalization.Configure(None))
      }

      "allow an app to declare empty port mappings" in {
        val raw = App(
          id = "/foo",
          cmd = Option("sleep"),
          container = Some(Container(
            `type` = EngineType.Mesos,
            portMappings = Option(Seq.empty
            ))),
          networks = Seq(Network(mode = NetworkMode.ContainerBridge)),
          unreachableStrategy = Option(UnreachableEnabled.Default)
        )
        raw.norm should be(raw)
      }

      "provide default port mappings when left unspecified for an app container w/ bridge networking" in {
        val raw = App(
          id = "/foo",
          cmd = Option("sleep"),
          container = Some(Container(
            `type` = EngineType.Mesos
          )),
          networks = Seq(Network(mode = NetworkMode.ContainerBridge)),
          unreachableStrategy = Option(UnreachableEnabled.Default)
        )
        raw.norm should be(raw.copy(container = raw.container.map(_.copy(
          portMappings = Option(Seq(ContainerPortMapping(hostPort = Option(0), name = Option("default"))))))))
      }

      "provide default port mappings when left unspecified for an app container w/ container networking" in {
        val raw = App(
          id = "/foo",
          cmd = Option("sleep"),
          container = Some(Container(
            `type` = EngineType.Mesos
          )),
          networks = Seq(Network(name = Option("network1"), mode = NetworkMode.Container)),
          unreachableStrategy = Option(UnreachableEnabled.Default)
        )
        raw.norm should be(raw.copy(container = raw.container.map(_.copy(
          portMappings = Option(Seq(ContainerPortMapping(name = Option("default"))))))))
      }

      "allow an app to declare empty port definitions" in {
        val raw = App(
          id = "/foo",
          cmd = Option("sleep"),
          portDefinitions = Option(Seq.empty),
          networks = Apps.DefaultNetworks,
          unreachableStrategy = Option(UnreachableEnabled.Default)
        )
        raw.norm should be(raw)
      }

      "provide a default port definition when no port definitions are specified" in {
        val raw = App(
          id = "/foo",
          cmd = Option("sleep"),
          networks = Apps.DefaultNetworks,
          unreachableStrategy = Option(UnreachableEnabled.Default)
        )
        raw.norm should be(raw.copy(portDefinitions = Option(Apps.DefaultPortDefinitions)))
      }
    }
  }

  private class Fixture {
    val legacyDockerApp = App(
      id = "/foo",
      container = Option(Container(
        `type` = EngineType.Docker,
        docker = Option(DockerContainer(
          network = Option(DockerNetwork.User),
          image = "image0",
          portMappings = Seq(ContainerPortMapping(
            containerPort = 1, hostPort = Option(2), servicePort = 3, name = Option("port0"), protocol = NetworkProtocol.Udp
          ))
        ))
      )),
      ipAddress = Option(IpAddress(
        networkName = Option("someUserNetwork")
      ))
    )

    val normalizedDockerApp = App(
      id = "/foo",
      container = Option(Container(
        `type` = EngineType.Docker,
        docker = Option(DockerContainer(
          image = "image0"
        )),
        portMappings = Option(Seq(ContainerPortMapping(
          containerPort = 1, hostPort = Option(2), servicePort = 3, name = Option("port0"), protocol = NetworkProtocol.Udp
        )))
      )),
      networks = Seq(Network(name = Option("someUserNetwork"))),
      unreachableStrategy = Option(UnreachableEnabled.Default)
    )

    val legacyMesosApp = App(
      id = "/foo",
      container = Option(Container(
        `type` = EngineType.Mesos,
        docker = Option(DockerContainer(image = "image0"))
      )),
      ipAddress = Option(IpAddress(
        networkName = Option("someUserNetwork"),
        discovery = Option(IpDiscovery(
          ports = Seq(IpDiscoveryPort(34, "port1", NetworkProtocol.Udp))
        ))
      ))
    )

    val normalizedMesosApp = App(
      id = "/foo",
      container = Option(Container(
        `type` = EngineType.Mesos,
        docker = Option(DockerContainer(image = "image0")),
        portMappings = Option(Seq(ContainerPortMapping(
          containerPort = 34, name = Option("port1"), protocol = NetworkProtocol.Udp
        )))
      )),
      networks = Seq(Network(name = Option("someUserNetwork"))),
      unreachableStrategy = Option(UnreachableEnabled.Default)
    )
  }
}
