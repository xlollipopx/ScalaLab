package cats

import cats.Task1.listM.pure

object Task1 {

  trait CustomMonad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f.andThen(pure))
  }

  val listM: CustomMonad[List] = new CustomMonad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case Nil => Nil
      case h :: t => f(h) ::: flatMap(t)(f)
    }

  }

  val optionM: CustomMonad[Option] = new CustomMonad[Option] {
    override def pure[A](a: A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(v) => (f(v))
      case None => None
    }

  }

  def main(args: Array[String]): Unit = {

    println(listM.flatMap(List(1,2,3))(x => List(x.toString + "a")))
    println(listM.flatMap(List(1,2,3))(x => List(x, x + 1)))
    val x = Option(3)
    print(optionM.flatMap(x)(x => Some("a")))

  }
}
