package typeclass

object typeClass {

  object TypeClassTask {

    trait HashCode[T] {
      def hash(value: T): Int
    }

    object HashCode {
      def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
    }

    implicit class HashCodeSyntax[T](x: T) {
      def hash(implicit hc: HashCode[T]): Int = {
       hc.hash(x)
      }
    }

    implicit val stringHash: HashCode[String] = string => string.hashCode
  }

  def main(args: Array[String]): Unit = {
    import TypeClassTask._
    print("abc".hash)

  }

}
