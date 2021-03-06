package async

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

object Exercise3 extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  val tasksCount     = 100
  val taskIterations = 1000
  val initialBalance = 10

  // PLACE TO FIX - START
  var balance1: Int = initialBalance
  var balance2: Int = initialBalance

  def doTaskIteration(): Unit = {
    synchronized {
      val State(newBalance1, newBalance2) = transfer(State(balance1, balance2))
      balance1 = newBalance1
      balance2 = newBalance2
    }
  }

  val stateAtomic = new AtomicReference[State](State(balance1, balance2))

  def doTaskIterationRef(): Unit = {
    val State(newBalance1, newBalance2) = stateAtomic.updateAndGet(transfer(_))
    balance1 = newBalance1
    balance2 = newBalance2
  }

  def printBalancesSum(): Unit = {
    println(balance1 + balance2)
  }
  // PLACE TO FIX - FINISH

  def transfer(state: State): State = {
    if (state.balance1 >= state.balance2) {
      State(state.balance1 - 1, state.balance2 + 1)
    } else {
      State(state.balance1 + 1, state.balance2 - 1)
    }
  }

  val tasks = (1 to tasksCount).toVector.map(_ =>
    Future {
      (1 to taskIterations).foreach(_ => doTaskIterationRef())
    }
  )
  val tasksResultFuture: Future[Vector[Unit]] = Future.sequence(tasks)
  Await.ready(tasksResultFuture, 5.seconds)

  printBalancesSum() //should print 20

  final case class State(balance1: Int, balance2: Int)
}
