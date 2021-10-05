//delete comments

//package cats_effects
//
//import cats.effect._
//import cats.effect.ExitCase
//import cats_effects.Effects.forever
//
//import scala.concurrent.duration._
//import scala.util.{Failure, Success, Try}
//import scala.util.Success
//import scala.io._
//
//object Fibers extends IOApp {
//
//  def processResultsFromFiber[A](io: IO[A]): IO[A] = for {
//    fiber  <- io.start
//    joined <- fiber.join
//    res    <- handleIO(joined)
//  } yield res
//
//  def handleIO[A](outcome: Outcome[IO, Throwable, A]): IO[A] = {
//    outcome match {
//      case Succeeded(x) => x
//      case Canceled()   => IO.raiseError(new RuntimeException("Canceled"))
//      case Errored(e)   => IO.raiseError(e)
//    }
//  }
//
//  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = for {
//    fiberA <- ioa.start
//    fiberB <- iob.start
//    resA   <- fiberA.join
//    resB   <- fiberB.join
//    res    <- handleTupleIOs((resA, resB))
//  } yield res
//
//  def handleTupleIOs[A, B](tuple: (Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])): IO[(A, B)] = {
//    tuple match {
//      case (Succeeded(x), Succeeded(y)) => sequenceIOs(x, y)
//      case (Canceled(), _)              => IO.raiseError(new RuntimeException("Canceled"))
//      case (_, Canceled())              => IO.raiseError(new RuntimeException("Canceled"))
//      case (_, Errored(e))              => IO.raiseError(e)
//      case (Errored(e), _)              => IO.raiseError(e)
//    }
//  }
//  def sequenceIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
//    for {
//      a <- ioa
//      b <- iob
//    } yield (a, b)
//  }
//
//  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = for {
//    res <- io.timeoutTo(duration, handleIO(Canceled()))
//  } yield res
//
//  def timeout2[A](io: IO[A], duration: FiniteDuration): IO[A] = for {
//    ioFib <- io.start
//    fiber <- (IO.sleep(duration) >> ioFib.cancel).start
//    _     <- fiber.join
//    resIO <- ioFib.join
//    res   <- handleIO(resIO)
//  } yield res
//
//  def forever[A](io: IO[A]): IO[A] = {
//    io.flatMap(x => forever(io))
//  }
//
//  def run(args: List[String]): IO[ExitCode] = for {
//    _ <- timeout2(IO.sleep(5.seconds) *> IO.raiseError(new RuntimeException("oh no")), 2.seconds)
//
//  } yield ExitCode.Success
//}
