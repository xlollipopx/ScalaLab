package tf

import cats.{Defer, Monad}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import cats.syntax.all._
import org.http4s.dsl.io.{->, /, BadRequest, Ok, POST, Root}
import tf.domain.employee.EmployeeDto
import tf.services.EmployeeService
import tf.validation.EmployeeValidator

object EmployeeHttp extends IOApp {

  final class BrandRoutes[F[_]: Defer: Monad: Sync](
  ) extends Http4sDsl[F] {

    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    private val routes: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "service" / "all" =>
      Ok {
        for {
          service <- EmployeeService.of[F](new EmployeeValidator)
          list    <- service.all
        } yield list
      }

    }

  }

  override def run(args: List[String]): IO[ExitCode] = ???
}
