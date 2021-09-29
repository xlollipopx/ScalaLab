package implicits

object Implicits {

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


    def main(args: Array[String]): Unit = {
      import Task1.instances._
      import Task1.syntax._
      import Exercise3.syntax._
      import Exercise3.instances._
      import Exercise3.hde._
      import Exercise3.CustomNumber

      42.show
      println("hello!".show)
      println(1, 313.show)
      println(Seq("I", "am", "a", "ghost").show)
      println(Seq(Seq(1, 2, 3), Seq(2)).show)
      println(Human("Denis").show)
      println(List(1,2,3).sumAll())
      println(List(CustomNumber(1), CustomNumber(2.23f),CustomNumber(2)).sumAll())
      println(secondBiggestValue(Seq(HDEYears(1), HDEYears(2), HDEYears(4),HDEYears(5),HDEYears(5))))
      println(secondBiggestValue(Seq()))
      print(List.empty[Int].sumAll())

    }



}
