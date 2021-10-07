package http

import cats.data.ZipList.catsDataCommutativeApplyForZipList.*>

import java.time.{Instant, LocalDate}
import cats.data.{EitherT, Validated}
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.instances.function.catsStdBimonadForFunction0.*>
import cats.syntax.all._
import http.Game.{StartInput, StartInputDto}
import http.GuessClient.uri
import http.GuessServer.{EQUAL, GREATER, NO_ATTEMPTS, SMALLER}
import json.Models.Post
import org.http4s.ParseResult.parseResultMonad.*>
import org.http4s.Query.blank.?
import org.http4s.Query.empty.?
import org.http4s.QueryParamEncoder.booleanQueryParamEncoder
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart.{Multipart, Part}
import org.http4s.server.blaze.BlazeServerBuilder

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext
import scala.util.Random

object Game {
  final case class StartInputDto(min: String, max: String, attempt: String)
  final case class StartInput(min: Int, max: Int, attempt: Int)
}

object GuessServer extends IOApp {

  object GuessNumberParamMatcher extends QueryParamDecoderMatcher[String]("number")

  private val guessedNumber = new AtomicReference[Int]
  private val attemptsLeft  = new AtomicReference[Int]
  private val min           = new AtomicReference[Int]
  private val max           = new AtomicReference[Int]
  private val isStarted     = new AtomicReference[Boolean](false)

  val NO_ATTEMPTS = "No attempts left!"
  val GREATER     = "Guessed number is greater!"
  val EQUAL       = "Congrats! You guessed the number!"
  val SMALLER     = "Guessed number is smaller!"

  def setUpValues(startInput: StartInput) = {
    val l    = startInput.min
    val r    = startInput.max
    val rand = l + Random.nextInt(r - l)
    guessedNumber.set(rand)
    attemptsLeft.set(startInput.attempt)
    min.set(l)
    max.set(r)
    isStarted.set(true)
    println(rand)
  }

  def handleGuess(num: String) = num.toIntOption match {
    case Some(_) if !isStarted.get()               => BadRequest("First start the game!")
    case Some(x) if x > max.get() || x < min.get() => BadRequest("Out of bounds!")
    case Some(_) if attemptsLeft.get() == 0        => Ok(NO_ATTEMPTS)
    case Some(x) if x == guessedNumber.get()       => Ok(EQUAL)
    case Some(x) if x < guessedNumber.get() =>
      Ok {
        attemptsLeft.getAndUpdate(x => x - 1)
        GREATER
      }
    case Some(x) if x > guessedNumber.get() =>
      Ok {
        attemptsLeft.getAndUpdate(x => x - 1)
        SMALLER
      }
    case Some(_) => Ok()
    case None    => BadRequest()
  }

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private val routes = HttpRoutes.of[IO] {

    case req @ POST -> Root / "start" =>
      req.as[StartInputDto].flatMap { dto =>
        StartInputValidator.validate(dto) match {
          case Right(x) =>
            Ok {
              setUpValues(x)
              "Game started!"
            }
          case Left(e) => BadRequest(e)
        }
      }

    case GET -> Root / "guess" :? GuessNumberParamMatcher(num) =>
      handleGuess(num)

  }

  private[http] val httpApp = {
    routes
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

}
object GuessClient extends IOApp {

  private val uri = uri"http://localhost:9001"
  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  val MIN      = "50"
  val MAX      = "100"
  val ATTEMPTS = "10"

  def guessNumber(client: Client[IO], min: Int, max: Int): IO[String] = for {
    mid <- IO((max - min) / 2 + min)
    ans <- client.expect[String]((uri / "guess").withQueryParam(key = "number", value = mid.toString))
    _    = println(ans)

    res <- ans match {
      case EQUAL       => IO(EQUAL + " " + mid)
      case GREATER     => guessNumber(client, mid + 1, max)
      case SMALLER     => guessNumber(client, min, mid - 1)
      case NO_ATTEMPTS => IO(NO_ATTEMPTS)
      case error       => IO(error)
    }

  } yield res

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO])
      .use { case (client, blocker) =>
        for {
          start <- {
            import io.circe.generic.auto._
            import org.http4s.circe.CirceEntityCodec._
            client.expect[String](Method.POST(StartInputDto(MIN, MAX, ATTEMPTS), uri / "start"))
          }
          _   <- printLine(start)
          res <- guessNumber(client, MIN.toInt, MAX.toInt)
          _   <- printLine(res)
        } yield ()
      }
      .as(ExitCode.Success)

}
