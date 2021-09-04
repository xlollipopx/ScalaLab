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
        val tempValues = values.sorted.distinct
        tempValues match {
          case _ :: tail if tempValues.size > 1 => Some(tail.head)
          case _ => None
        }

      }

    }
    case class CustomNumber(value: Float)

    trait Summable[T] {
      def apply(a: T, b: T): T
    }

    def +[T](a: T, b: T)(implicit summable: Summable[T]): T = {
      summable.apply(a, b)
    }

    object syntax {
      implicit class SummableOps[T: Summable](seq: Seq[T]) {
        def sumAll(): T = seq.reduceLeft((sum, el) => Exercise3.+(sum, el))
      }

    }
    object instances {
      implicit val numericAdd: Summable[Int] = (a: Int, b: Int) => a + b

      implicit val customNumberAdd: Summable[CustomNumber] = (a: CustomNumber, b: CustomNumber) =>
        CustomNumber(a.value + b.value)
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
      println(secondBiggestValue(Seq(HDEYears(1), HDEYears(2), HDEYears(3))))


    }


}
