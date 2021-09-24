package cats

import scala.util.{Failure, Success}

object Task2 {

  /** Ex 7.0 implement traverse function for Option
    */
  def optionTraverse[A](input: List[Option[A]]): Option[List[A]] = {
    input.foldRight(Option(List[A]()))((list, el) =>
      el match {
        case Some(x) => Some(list.get :: x)
        case None    => None
      }
    )
  }

  /** Ex 7.1 implement traverse for Either. Use fail fast approach (the first error encountered is returned.)
    */
  def eitherTraverse[E, A](input: List[Either[E, A]]): Either[E, List[A]] = {
    input.foldRight(Right(List[A]()))((list, el) =>
      el match {
        case Right(x) if list.toOption == None => Right(x)
        case Right(x)                          => Right(list.toOption.get :: x)
        case x                                 => x
      }
    )
  }

  import cats.syntax.functor._
  def increment_v2[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)

  trait Tree[+T]
  object Tree {
    def leaf[T](value:   T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  object FunctorTree extends Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      case Leaf(value)                => Leaf(f(value))
    }
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  def main(args: Array[String]): Unit = {

//    println(optionTraverse(List(Option(1), Option(2), Option(19))))
//    println(optionTraverse(List(Option(1), Option(2), Option(None))))
//    println(eitherTraverse(List(Right(1), Right(4), Right(6))))
//    println(eitherTraverse(List(Right(1), Right(3), Left(4))))
//    val leafOne   = Leaf(1)
//    val leafTwo   = Leaf(2)
//    val branchOne = Branch(10, leafOne, leafTwo)
//    print(FunctorTree.map(branchOne)(x => 2 * x))
    val p = println("lksv")

  }

}
