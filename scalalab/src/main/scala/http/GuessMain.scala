package http

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import http.Game.StartInput
import http.core.ClientEngine.guessNumber
import http.routes.GuessRoutes
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import org.http4s.circe.jsonOf
import org.http4s.{Method, Uri}
import org.http4s.client.dsl.io._
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io.POST
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

object Game {
  @JsonCodec final case class StartInput(min: Int, max: Int, attempts: Int)
  final case class GuessGame(guessedNumber: Int, attemptsLeft: Int, min: Int, max: Int, isStarted: Boolean = false)
}

object guessServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(GuessRoutes[IO]().httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

}

object GuessClient extends IOApp {
  val MIN         = 50
  val MAX         = 100
  val ATTEMPTS    = 10
  private val uri = uri"http://localhost:9001"
  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO])
      .use { case (client, blocker) =>
        for {
          start <- {
            import org.http4s.circe.CirceEntityCodec._
            val req = POST(StartInput(MIN, MAX, ATTEMPTS).asJson, uri / "start")
            client.expect(req)(jsonOf[IO, String])
          }
          _   <- printLine(start)
          res <- guessNumber(client, MIN, MAX, uri)
          _   <- printLine(res)
        } yield ()
      }
      .as(ExitCode.Success)
}
