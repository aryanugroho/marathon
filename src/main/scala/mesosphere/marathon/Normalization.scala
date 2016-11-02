package mesosphere.marathon

trait Normalization[T] extends AnyRef {
  def normalized(t: T): T
}

object Normalization {

  implicit class Normalized[T](val a: T) extends AnyVal {
    def norm(implicit f: Normalization[T]): T = f.normalized(a)
  }

  def apply[T](f: (T => T)): Normalization[T] = new Normalization[T] {
    override def normalized(t: T): T = f(t)
  }
}
