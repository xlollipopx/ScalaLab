package tf.modules

import cats.effect.Async
import cats.syntax.all._
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware._
import cats.effect.IO._
import tf.http.HttpEmployeeRoutes
import tf.validation.EmployeeValidator
import org.http4s.implicits._
import tf.services.http.HttpEmployeeService

object HttpApi {
  def make[F[_]: Async](employeeService: HttpEmployeeService[F]): HttpApi[F] =
    new HttpApi[F](employeeService) {}
}

sealed abstract class HttpApi[F[_]: Async](
  employeeService: HttpEmployeeService[F]
) {

  val employeeRoutes = HttpEmployeeRoutes[F](employeeService).routes

  private val middleware: HttpRoutes[F] => HttpRoutes[F] = {
    { http: HttpRoutes[F] =>
      AutoSlash(http)
    } andThen { http: HttpRoutes[F] =>
      CORS(http)
    }
  }

  val httpApp: HttpApp[F] = middleware(employeeRoutes).orNotFound

}
