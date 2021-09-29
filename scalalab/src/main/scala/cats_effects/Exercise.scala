package cats_effects

import java.util.concurrent.atomic.AtomicBoolean
import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn
import cats.implicits._

import scala.annotation.tailrec
import scala.concurrent.{duration, Await, Future}
import scala.concurrent.duration._
import scala.util.{Random, Try}

trait Console {
  def putStrLn(value: String): IO[Unit]
  def readStrLn: IO[String]
}

object Console {
  object Real extends Console {
    def putStrLn(value: String): IO[Unit] = IO(println(value))
    def readStrLn: IO[String] = IO(StdIn.readLine())
  }
}

object Exercise1_Common {
  def response(animal: String): Option[String] = animal.trim match {
    case "cat" | "cats"  => "In ancient times cats were worshipped as gods; they have not forgotten this.".some
    case "dog" | "dogs"  => "Be the person your dog thinks you are.".some
    case x if x.nonEmpty => s"I don't know what to say about '$x'.".some
    case _               => none
  }
}

object Exercise1_Functional extends IOApp {
  import Exercise1_Common._

  def process(console: Console, counter: Int = 0): IO[ExitCode] = for {
    _     <- IO(println("What is your favourite animal?"))
    animal = StdIn.readLine()
    output = response(animal)
    _     <- (getOutput(output, console, counter))
  } yield ExitCode.Success

  def getOutput(output: Option[String], console: Console, counter: Int = 0): IO[Either[Unit, ExitCode]] =
    output match {
      case Some(x) =>
        IO(Left(println(x)))
      case None =>
        if (counter >= 2) {
          IO(Left(println("I am disappoint. You have failed to answer too many times.")))
        } else {
          for {
            _   <- IO(println("Empty input is not valid, try again..."))
            res <- process(console, counter + 1)
          } yield Right(res)
        }
    }

  override def run(args: List[String]): IO[ExitCode] = process(Console.Real)
}
