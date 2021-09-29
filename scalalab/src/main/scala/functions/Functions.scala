package functions

import scala.annotation.tailrec

object Functions {

  // https://www.codewars.com/kata/555eded1ad94b00403000071/train/scala
  def seriesSum(n: Int): String = {
    val generateSeq: Int => List[Double] = List.tabulate(_)(x => 1 / (3 * x.toDouble + 1))
    val round: Double => Double = "%.2f".format(_).toDouble
    val genStr = round(generateSeq(n).sum).toString
    genStr.length match {
      case 3 => genStr + "0"
      case _ => genStr
    }
  }

  // https://www.codewars.com/kata/525f50e3b73515a6db000b83/train/scala
  def createPhoneNumber(numbers: Seq[Int]): String = {
    val toStr: Seq[Int] => String = _.foldLeft("")((acc, x) => acc + x)
    "(" + toStr(numbers.slice(0, 3)) + ") " + toStr(numbers.slice(3, 6)) + "-" + toStr(numbers.slice(6, 11))
  }

  // https://www.codewars.com/kata/550498447451fbbd7600041c/train/scala
  def comp(seq1: Seq[Int], seq2: Seq[Int]) = (seq1, seq2) match {
    case (_, Nil) => false
    case (Nil, _) => false
    case (s1, s2) => s1.map { x => x * x }.sorted == s2.sorted
  }

  // https://www.codewars.com/kata/51ba717bb08c1cd60f00002f/train/scala
  def solution(xs: List[Int]): String = {
    if (xs.isEmpty) return ""
    val groupList: List[Int] => List[List[Int]] = {
      @tailrec
      def group(list: List[Int], acc: List[Int], result: List[List[Int]])
               (f: (Int, Int) => Boolean):
      List[List[Int]] = list match {
        case Nil => acc.reverse :: result
        case head :: tail =>
          acc match {
            case Nil => group(tail, head :: acc, result)(f)
            case accHead :: _ if f(head, accHead) =>
              group(tail, head :: acc, result)(f)
            case _ =>
              group(tail, head :: List(), acc.reverse :: result)(f)
          }
      }

      group(_, List(), List())((x, y) => x - y == 1).reverse
    }
    groupList(xs).map { x =>
      x.length match {
        case 1 => x.head.toString
        case 2 => x.head.toString + "," + x.last.toString
        case _ => x.head.toString + "-" + x.last.toString
      }
    }.mkString(",")
  }


  // https://www.codewars.com/kata/556deca17c58da83c00002db/train/scala
  def tribonacci[T: Numeric](signature: List[T], n: Int): List[T] = {
    @tailrec
    def tailTrib[T](n: Int, preprev: T, prev: T, cur: T)(acc: List[T])(implicit num: Numeric[T]): List[T] = {
      import num._
      if (n > 0) tailTrib(n - 1, preprev = preprev + prev + cur, prev = preprev, cur = prev)(cur :: acc)
      else acc
    }

    tailTrib(n, signature.last, signature(1), signature.head)(List()).reverse
  }

  def tribonacci1[T: Numeric](sig: List[T], n: Int): List[T] = n match {
    case n if n <= 3 => sig take n
    case _ => sig.head +: tribonacci1(sig.tail :+ sig.sum, n - 1)
  }

  //https://www.codewars.com/kata/529bf0e9bdf7657179000008/train/scala
  def validate(matrix: Array[Array[Int]]): Boolean = {
    val nums = List.tabulate(9)(x => x + 1)
    val list = matrix.toList.map(_.toList)
    val check: List[List[Int]] => Boolean =
      _.foldLeft(true)((acc, x) => acc & nums == x.sorted)
    val transported = List.tabulate(9, 9) { (r, c) => list(c)(r) }
    val dividedMatrix = List.tabulate(9, 9) { (r, c) =>
      list(r / 3 * 3 + c / 3)(r % 3 * 3 + c % 3)
    }
    check(list) && check(transported) && check(dividedMatrix)
  }

    def main(args: Array[String]): Unit = {


    }
}