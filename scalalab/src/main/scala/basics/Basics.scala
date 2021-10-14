package basics

import scala.annotation.tailrec

object Basics {
  def fibonacci(n: Int): Either[String, Int] = {
    if (n < 0) return Left("n must be non-negative")

    @tailrec
    def tailFib(n: Int, prev: Int, cur: Int): Int =
      if (n > 0) tailFib(n - 1, prev = prev + cur, cur = prev)
      else cur

    Right(tailFib(n, prev = 1, cur = 0))
  }

  def atkinSieve(n: Int): Either[String, List[Int]] = {
    if (n < 2) return Left("n must be not less then 2")

    val sqrt    = Math.sqrt(n)
    val squareX = List.tabulate(sqrt.toInt)(x => x * x)
    val squareY = List.tabulate(sqrt.toInt)(y => y * y)
    val listOne = templateFunc((x, y) => 4 * x + y, squareX, squareY, (_, _) => true)

    val numType1 = listOne.filter(x => (x <= n && (x % 12 == 1 || x % 12 == 5)))
    val listTwo  = templateFunc((x, y) => 3 * x + y, squareX, squareY, (_, _) => true)

    val numType2  = listTwo.filter(x => (x <= n && x % 12 == 7))
    val listThree = templateFunc((x, y) => 3 * x - y, squareX, squareY, (x, y) => x > y)

    val numType3 = listThree.filter(x => (x <= n && x % 12 == 11))

    val numbers = numType1 ++ numType2 ++ numType3
    numbers.filter(x => numbers.count(_ == x) % 2 == 1)

    val resT = numbers.distinct
    val nums = List.tabulate(sqrt.toInt / 2 + 2)(x => 2 * x + 1).filter(_ >= 5)

    val set = for {
      x   <- resT
      num <- nums
      if x % num == 0
    } yield x
    val res = resT.filter(!set.contains(_))

    val result = n match {
      case 2 => 2 :: res.toList
      case _ => 2 :: 3 :: res.toList
    }
    Right(result.sorted.tail)
  }

  def templateFunc(
    f:       (Int, Int) => Int,
    squareX: List[Int],
    squareY: List[Int],
    comp:    (Int, Int) => Boolean
  ): List[Int] = {
    for {
      x <- squareX
      y <- squareY
      if comp(x, y)
    } yield f(x, y)
  }

  def main(args: Array[String]): Unit = {

    print(atkinSieve(100))
  }
}
