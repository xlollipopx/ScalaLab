import scala.annotation.tailrec
import scala.collection.{BitSet, mutable}

object Main {


  def fibonacci(n: Int): Either[String, Int]  = {
    if(n < 0) {
      return Left("n must be non-negative")
    }
    @tailrec
    def tailFib(n: Int, prev: Int, cur: Int): Int = {
      if (n > 0) {
        tailFib(n - 1, prev = prev + cur, cur = prev)
      } else {
        cur
      }
    }
    Right(tailFib(n, prev = 1, cur = 0))
  }

  def atkinSieve(n: Int): Either[String, List[Int]] = {
    if(n < 2){
     return  Left("n must be not less then 2")
    }

    val sqrt = Math.sqrt(n)
    val sieve = mutable.HashMap[Int, Int]()
    for(p <- 1 to n) {
      sieve.put(p, 0)
    }

    for( x <- 1 to sqrt.toInt) {
      for(y <- 1 to sqrt.toInt){
        val x2 = x * x
        val y2 = y * y
        val k = 4 * x2 + y2
        if (k <= n && (k%12 == 1 || k%12 == 5)){
          val flag = sieve(k)
          sieve.update(k, (flag + 1)%2)
        }
        val k1 = k - x2
        if ( k1 <= n && k1%12 == 7){
          val flag = sieve(k1)
          sieve.update(k1, (flag + 1)%2)
        }
        val k2 = k1 - 2 * y2
        if (x > y && k2 <= n && k2%12 == 11){
          val flag = sieve(k2)
          sieve.update(k2, (flag + 1)%2)
        }
      }
    }

    for(i <- 5 to sqrt.toInt by 2){
      if(sieve(i) %2 == 0) {
       val sqr = i * i
       for(j <- sqr to n by sqr){
         sieve.update(j, 0)
       }
      }
    }
    val tmpRes = sieve.keySet.filter(sieve(_) %2 == 1)

    val result = n match {
      case 2 =>  2::tmpRes.toList
      case _ => 2::3::tmpRes.toList
    }
    Right(result.sorted)
  }


  def main(args: Array[String]): Unit = {

    println(fibonacci(10))
    println(atkinSieve(20))
  }
}
