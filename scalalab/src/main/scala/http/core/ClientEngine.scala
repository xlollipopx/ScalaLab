package http.core

import cats.Monad
import cats.effect.ConcurrentEffect
import cats.implicits.catsSyntaxApplicativeId
import http.routes.meta.Result.{EQUAL, GREATER, NO_ATTEMPTS, SMALLER}
import org.http4s.client.Client
import cats.implicits._
import org.http4s.Uri

object ClientEngine {

  def guessNumber[F[_]: Monad: ConcurrentEffect](client: Client[F], min: Int, max: Int, uri: Uri): F[String] = for {
    mid <- ((max - min) / 2 + min).pure[F]

    ans <- client.expect[String]((uri / "guess").withQueryParam(key = "number", value = mid.toString))
    _    = println(ans)

    res <- ans match {
      case _ if ans.contains(EQUAL)       => (EQUAL + " " + mid).pure[F]
      case _ if ans.contains(GREATER)     => guessNumber(client, mid + 1, max, uri)
      case _ if ans.contains(SMALLER)     => guessNumber(client, min, mid - 1, uri)
      case _ if ans.contains(NO_ATTEMPTS) => NO_ATTEMPTS.pure[F]
      case error                          => error.pure[F]
    }

  } yield res
}
