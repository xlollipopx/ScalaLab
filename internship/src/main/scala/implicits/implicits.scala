package implicits

object implicits {

  case class Human(name: String)

  object Task1 {
    trait Show[T] {
      def apply(value: T): String
    }

//    def show[T: Show](value: T): String =
//      implicitly[Show[T]].apply(value)
def show[T](value: T)(implicit showable: Show[T]): String =
  showable.apply(value)

    object syntax {
      implicit class ShowOps[T: Show](inner: T){
        def show: String = Task1.show(inner)
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
    /**
     * Amount of years since the invention of the
     * hyper-drive technology (we are certainly in negative values at the moment).
     */

    object implicits {


      def secondBiggestValue[T](values: Seq[T])(implicit ordering: Ordering[T]): Option[T] = {
        ordering
      }

      object syntax{

      }

      object instances{

      }

    }

    case class HDEYears(value: Long)

    /*
    should be defined on any T which has Ordering[T] and return second biggest value from the sequence
    if it exists
    should work on our custom HDEYears
    change the signature accordingly, add implicit instances if needed
     */
    def secondBiggestValue[T](values: Seq[T]): Option[T] = ???


    /**
     * Custom number type!
     * For now it just wraps a Float but more interesting stuff could come in the future, who knows...
     */
    case class CustomNumber(value: Float)

    /*
    should be defined on any T which has Summable[T], should return sum value if it can be obtained
    should work on our custom CustomNumber
    change the signature accordingly, add implicit instances if needed
     */
    def sum[T](values: Seq[T]): Option[T] = ???
  }

  def main(args: Array[String]): Unit = {
    import Task1.instances._
    import Task1.syntax._

    println(42.show)
    println("hello!".show)
    println(1,313.show)
    println(Seq("I", "am", "a", "ghost").show)
    println(Seq(Seq(1,2,3), Seq(2)).show)
    println(Human("Denis").show)
  }


}
