package cats_effects

import java.util.concurrent.atomic.AtomicBoolean
import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn
import cats.implicits._

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Random, Try}
import cats.effect.IO

object Effects extends IOApp {

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    for {
      _   <- ioa
      res <- iob
    } yield res
  }

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    for {
      res <- ioa
      _   <- iob
    } yield res
  }

  def run(args: List[String]): IO[ExitCode] = for {
    _ <- sequenceTakeLast(IO(println(23)), IO(10))
    _ <- sequenceTakeFirst(IO(21), IO(println("44")))
    _ <- forever(IO(println(3)))
  } yield ExitCode.Success

  // 3 - repeat an IO effect forever
  // hint: use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] = {
    io.flatMap(x => forever(io))
  }

  // 4 - convert an IO to a different type
  // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = {
    ioa.map(x => value)
  }

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = IO()
}
