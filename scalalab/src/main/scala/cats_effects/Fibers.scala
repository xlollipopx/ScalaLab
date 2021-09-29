package cats_effects

import cats.effect._
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.util.Success
import scala.io._

object Fibers extends IOApp {

  def processResultsFromFiber[A](io: IO[A]): IO[A] = for {
    fiber  <- io.start
    joined <- fiber.join
    res    <- handleIO(joined)
  } yield res

  def handleIO[A](outcome: Outcome[IO, Throwable, A]): IO[A] = {
    outcome match {
      case Succeeded(x) => x
      case Canceled()   => IO.raiseError(new RuntimeException("Canceled"))
      case Errored(e)   => IO.raiseError(e)
    }
  }

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = for {
    fiberA <- ioa.start
    fiberB <- iob.start
    resA   <- fiberA.join
    resB   <- fiberB.join
    res    <- handleTupleIOs((resA, resB))
  } yield res

  def handleTupleIOs[A, B](tuple: (Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])): IO[(A, B)] = {
    tuple match {
      case (Succeeded(x), Succeeded(y)) => sequenceIOs(x, y)
      case (Canceled(), _)              => IO.raiseError(new RuntimeException("Canceled"))
      case (_, Canceled())              => IO.raiseError(new RuntimeException("Canceled"))
      case (_, Errored(e))              => IO.raiseError(e)
      case (Errored(e), _)              => IO.raiseError(e)
    }
  }
  def sequenceIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    for {
      a <- ioa
      b <- iob
    } yield (a, b)
  }

//  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = for {
//    i <- io.can
//
//  } yield ()

  def run(args: List[String]): IO[ExitCode] = { IO(ExitCode.Success) }
}
