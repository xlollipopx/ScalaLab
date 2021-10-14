package http.routes

import cats.Monad
import cats.effect.Sync
import http.Game.{GuessGame, StartInput}
import org.http4s.server.Router
import org.http4s.implicits._
import cats.implicits._
import http.routes.meta.Result.{EQUAL, GREATER, NO_ATTEMPTS, SMALLER}
import http.routes.meta.Setup.setUpValues
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityCodec._

import java.util.concurrent.atomic.AtomicReference

final case class GuessRoutes[F[_]: Sync: Monad]() extends Http4sDsl[F] {
  object GuessNumberParamMatcher extends QueryParamDecoderMatcher[String]("number")
  private val guessGame = new AtomicReference[GuessGame]

  private val routes = HttpRoutes.of[F] {
    case req @ POST -> Root / "start" =>
      for {
        input <- {
          req.as[StartInput]
        }
        resp <- Ok {
          setUpValues(guessGame, input)
          "Game started!"
        }
      } yield resp

    case GET -> Root / "guess" :? GuessNumberParamMatcher(num) =>
      handleGuess(num)
  }

  val httpApp: HttpApp[F] = {
    routes
  }.orNotFound

  private def handleGuess(num: String) = num.toIntOption match {
    case Some(_) if !guessGame.get().isStarted                         => BadRequest("First start the game!")
    case Some(x) if x > guessGame.get().max || x < guessGame.get().min => BadRequest("Out of bounds!")
    case Some(_) if guessGame.get().attemptsLeft == 0                  => Ok(NO_ATTEMPTS)
    case Some(x) if x == guessGame.get().guessedNumber                 => Ok(EQUAL)
    case Some(x) if x < guessGame.get().guessedNumber =>
      Ok {
        guessGame.getAndUpdate(x => GuessGame(x.guessedNumber, x.attemptsLeft - 1, x.min, x.max, x.isStarted))
        GREATER
      }
    case Some(x) if x > guessGame.get().guessedNumber =>
      Ok {
        guessGame.getAndUpdate(x => GuessGame(x.guessedNumber, x.attemptsLeft - 1, x.min, x.max, x.isStarted))
        SMALLER
      }
    case Some(_) => Ok()
    case None    => BadRequest()
  }
}
