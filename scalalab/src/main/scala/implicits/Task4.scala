package implicits

object Task4 {
  object Exercise4 {

    trait Summable[T] {
      def plus(a: T, b: T): T
      def zero(): T
    }

    def +[T](a: T, b: T)(implicit summable: Summable[T]): T = {
      summable.plus(a, b)
    }

    def empty[T]()(implicit summable: Summable[T]): T = {
      summable.zero()
    }

    def fold[F[_]: Foldable, T: Summable](
      seq: F[T],
      s:   T,
      f:   (T, T) => T
    )(
      implicit summable: Summable[T],
      foldable:          Foldable[F]
    ): T = {
      foldable.foldLeft(seq, s)(f)
    }

    object summableSyntax {

      implicit class SummableOps[F[T]: Foldable, T: Summable](seq: F[T]) {
        def genericSum(s: T)(f: (T, T) => T): T = {
          Exercise4.fold(seq, s, f)
        }
      }
    }

    object summableInstances {

      implicit def numericAdd[T: Numeric](implicit num: Numeric[T]): Summable[T] = new Summable[T] {
        import num._
        def plus(a: T, b: T): T = a + b
        def zero(): T = num.zero
      }

      implicit def setAdd[S](): Summable[Set[S]] = new Summable[Set[S]] {
        def plus(a: Set[S], b: Set[S]): Set[S] = a ++ b
        def zero(): Set[S] = Set.empty[S]
      }

      implicit def listAdd[S](): Summable[List[S]] = new Summable[List[S]] {
        def plus(a: List[S], b: List[S]): List[S] = a ++ b
        def zero(): List[S] = List.empty[S]
      }
    }

    case class Triple[T](
      v1: T,
      v2: T,
      v3: T,
    )

    trait Foldable[F[_]] {
      def foldLeft[T, S](ft: F[T], s: S)(f: (S, T) => S): S
    }

    object foldInstances {
      implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
        override def foldLeft[T, S](ft: Option[T], s: S)(f: (S, T) => S): S =
          ft match {
            case None    => s
            case Some(t) => f(s, t)
          }
      }

      implicit val listFoldable: Foldable[List] = new Foldable[List] {
        override def foldLeft[T, S](ft: List[T], s: S)(f: (S, T) => S): S = {
          ft.foldLeft(s)(f)
        }
      }

      implicit val tripleFoldable: Foldable[Triple] = new Foldable[Triple] {
        override def foldLeft[T, S](ft: Triple[T], s: S)(f: (S, T) => S): S = {
          val list = List(ft.v1, ft.v2, ft.v2)
          list.foldLeft(s)(f)
        }
      }
    }

  }

  def main(args: Array[String]): Unit = {
    import Exercise4.summableInstances._
    import Exercise4.summableSyntax._
    import Exercise4.foldInstances._
    println(List(1, 2, 3).genericSum(0)((a: Int, b: Int) => a + b))
  }

}
