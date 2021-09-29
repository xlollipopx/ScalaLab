package basics

import scala.collection.immutable.ListMap
import scala.runtime.Nothing$

object DataStructures {

  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = {
    set match {
      case set if n == 1 => set.map(x => Set(x))
      case set if set.nonEmpty =>
        allSubsetsOfSizeN(set.tail, n - 1).map(x => x ++ Set(set.head)) ++ allSubsetsOfSizeN(set.tail, n)
      case _ => Set.empty
    }
  }
  def multiply(a: Int, b: Int): Int = {a * b}

  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    val sorted = ListMap(map.toSeq.sortWith { case((_,b),(_,d)) =>
      b < d
    }: _*)

    sorted.
      toList.
      groupBy { case (_, b) => b }.view.
      mapValues(_.map { case (x, _) => x }.toSet).
      map { case (x, y) => (y, x) }.toList
  }


  def main(args: Array[String]): Unit = {
    val a = Set(1, 2, 3, 4, 5)
    println(allSubsetsOfSizeN(a, 3))
    val m = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val res = sortConsideringEqualValues(m)
    print(res)
  }

}
