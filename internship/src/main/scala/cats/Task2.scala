package cats

object Task2 {
  /** Ex 7.0 implement traverse function for Option
   */
  def optionTraverse[A](input: List[Option[A]]): Option[List[A]] = input match {
    case Nil => Some(Nil)
    case h :: t => h match {
      case None => None
      case Some(head) => optionTraverse(t) match {
        case None => None
        case Some(list) => Some(head :: list)
      }
    }
  }

  /** Ex 7.1 implement traverse for Either. Use fail fast approach (the first error encountered is returned.)
   */
  def eitherTraverse[E, A](input: List[Either[E, A]]): Either[E, List[A]] = input match {
    case Nil => Right(Nil)
    case h :: t => h match {
      case Left(x) => Left(x)
      case Right(head) => eitherTraverse(t) match {
        case Left(x) => Left(x)
        case Right(list) => Right(head :: list)
      }
    }
  }

  def main(args: Array[String]): Unit = {

    println(optionTraverse(List(Option(1), Option(2), Option(19))))
    println(optionTraverse(List(Option(1), Option(2), Option(None))))
    println(eitherTraverse(List(Right(1), Right(4), Right(6))))
    println(eitherTraverse(List(Right(1), Right(3), Left(4))))
  }
}
