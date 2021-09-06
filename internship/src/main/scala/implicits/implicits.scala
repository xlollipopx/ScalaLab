package implicits

object implicits {

  case class Human(name: String)

  object Task1 {

    trait Show[T] {
      def apply(value: T): String
    }

    def show[T](value: T)(implicit show: Show[T]): String =
      show.apply(value)

    object syntax {
      implicit class ShowOps[T: Show](inner: T) {
        def show: String = Task1.show(inner).reverse
      }
    }

    object instances {
      implicit val stringShow: Show[String] = (value: String) => value
      implicit val intShow: Show[Int] = (value: Int) => value.toString
      implicit def seqShow[T: Show]: Show[Seq[T]] = {
        (value: Seq[T]) => value.map(show(_)).mkString("(", ", ", ")")
      }
      implicit val humanShow: Show[Human] = (value: Human) => "Name:" + value.name
    }

  }

  /*
  Exercise 3.
  There are some type-classes in Scala standard library.
  Let's get to know them better!
   */
  object Exercise3 {

    object hde {

      case class HDEYears(value: Long)

      implicit def ordering[A <: HDEYears]: Ordering[A] =
        Ordering.by(e => (e.value, e.value))


      def secondBiggestValue[T](values: Seq[T])(implicit ordering: Ordering[T]): Option[T] = {
        val tempValues = values.sorted(Ordering[T].reverse).distinct
        tempValues match {
          case _ :: tail if tempValues.size > 1 => Some(tail.head)
          case _ => None
        }

      }

    }
    case class CustomNumber(value: Float)

    trait Summable[T] {
      def apply(a: T, b: T): T
      def empty(): T
    }

    def +[T](a: T, b: T)(implicit summable: Summable[T]): T = {
      summable.apply(a, b)
    }

    def empty[T]()(implicit summable: Summable[T]): T = {
      summable.empty()
    }

    object syntax {
      implicit class SummableOps[T: Summable](seq: Seq[T]) {
        def sumAll(): T =
          seq.fold(Exercise3.empty())(Exercise3.+)
      }

    }
    object instances {
      implicit val numericAdd: Summable[Int] = new Summable[Int] {
        override def apply(a: Int, b: Int): Int = a + b
        override def empty(): Int = 0
      }



      implicit val customNumberAdd: Summable[CustomNumber] = new Summable[CustomNumber] {
        override def apply(a: CustomNumber, b: CustomNumber): CustomNumber = CustomNumber(a.value + b.value)
        override def empty(): CustomNumber = CustomNumber(0)

      }
    }

    }

  object Exercise4 {

    /*
    Generic foldLeft!
    F[_] - type constructor with a single type argument, like List[T], Option[T], etc.
    Types which are parameterized using type constructors called higher-kinded types (HKT)
    Foldable here is a HKT
     */

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

    def fold[ F[_]: Foldable, T: Summable](seq: F[T], s: T, f: (T, T) => T )
                                          (implicit summable: Summable[T], foldable: Foldable[F]): T ={
      foldable.foldLeft(seq, s)(f)
    }

    object summableSyntax {

      implicit class SummableOps[F[T]: Foldable, T: Summable](seq: F[T]) {
        def genericSum(s: T) (f: (T, T) => T): T = {
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
        def zero():  Set[S] = Set.empty[S]
      }

      implicit def listAdd[S](): Summable[List[S]] = new Summable[List[S]] {
        def plus(a: List[S], b: List[S]): List[S] = a ++ b
        def zero():  List[S] = List.empty[S]
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


    object foldInstances{
      implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
        override def foldLeft[T, S](ft: Option[T], s: S)(f: (S, T) => S): S =
          ft match {
            case None => s
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
      import Task1.instances._
      import Task1.syntax._
      import Exercise3.syntax._
      import Exercise3.instances._
      import Exercise3.hde._
      import Exercise3.CustomNumber
      import Exercise4.summableInstances._
      import Exercise4.summableSyntax._
      import Exercise4.foldInstances._

//      42.show
//      println("hello!".show)
//      println(1, 313.show)
//      println(Seq("I", "am", "a", "ghost").show)
//      println(Seq(Seq(1, 2, 3), Seq(2)).show)
//      println(Human("Denis").show)
//      println(List(1,2,3).sumAll())
//      println(List(CustomNumber(1), CustomNumber(2.23f),CustomNumber(2)).sumAll())
//      println(secondBiggestValue(Seq(HDEYears(1), HDEYears(2), HDEYears(4),HDEYears(5),HDEYears(5))))
//      println(secondBiggestValue(Seq()))
//      print(List.empty[Int].sumAll())

       println(List(1,2,3).genericSum(0)((a: Int, b: Int) => a + b))

    }



}
