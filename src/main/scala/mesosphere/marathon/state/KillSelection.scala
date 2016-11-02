package mesosphere.marathon
package state

/**
  * Defines a kill selection for tasks. See [[mesosphere.marathon.core.deployment.ScalingProposition]].
  */
sealed trait KillSelection {
  def apply(a: Timestamp, b: Timestamp): Boolean = this match {
    case KillSelection.YoungestFirst => a.youngerThan(b)
    case KillSelection.OldestFirst => a.olderThan(b)
  }
  val value: String

  def toProto: Protos.KillSelection
}

object KillSelection {

  case object YoungestFirst extends KillSelection {
    override val value = raml.KillSelection.YoungestFirst.value
    override def toProto: Protos.KillSelection =
      Protos.KillSelection.YoungestFirst
  }
  case object OldestFirst extends KillSelection {
    override val value = raml.KillSelection.OldestFirst.value
    override def toProto: Protos.KillSelection =
      Protos.KillSelection.OldestFirst
  }

  def DefaultKillSelection: KillSelection = raml.KillSelection.DefaultValue.fromRaml

  def fromProto(proto: Protos.KillSelection): KillSelection = {
    proto match {
      case Protos.KillSelection.YoungestFirst =>
        YoungestFirst
      case Protos.KillSelection.OldestFirst =>
        OldestFirst
    }
  }
}
