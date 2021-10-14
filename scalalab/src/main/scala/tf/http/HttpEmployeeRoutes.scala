package tf.http

import cats.{Defer, Monad}
import cats.effect.Sync
import org.http4s.dsl.Http4sDsl
import cats.syntax.all._
import org.http4s.server.Router
import org.http4s.circe._
import io.circe.generic.auto._
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import tf.dto.employee.EmployeeDto
import tf.services.http.HttpEmployeeService
import io.circe.syntax._

final case class HttpEmployeeRoutes[F[_]: Monad: Sync](employeeService: HttpEmployeeService[F]) extends Http4sDsl[F] {
  import org.http4s.circe.CirceEntityEncoder._
  private val prefixPath = "/employees"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "all" =>
      for {
        list <- employeeService.all
        res  <- Ok(list.asJson)
      } yield res

    //localhost:9001/employees/find/23db588e-ef81-485e-9e00-b7ec65e727c6
    case GET -> Root / "find" / UUIDVar(id) =>
      for {
        employee <- employeeService.find(id)
        res      <- Ok(employee.asJson)
      } yield res

    case req @ POST -> Root / "create" =>
      req.as[EmployeeDto].flatMap { dto =>
        for {
          employee <- employeeService.create(dto)
          res      <- Ok(employee.asJson)
        } yield res
      }

    //localhost:9001/employees/delete/23db588e-ef81-485e-9e00-b7ec65e727c6
    case DELETE -> Root / "delete" / UUIDVar(id) =>
      for {
        ans <- employeeService.delete(id)
        res <- Ok(ans.asJson)
      } yield res

    case req @ PUT -> Root / "update" / UUIDVar(id) =>
      req.as[EmployeeDto].flatMap { dto =>
        for {
          employee <- employeeService.update(id, dto)
          res      <- Ok(employee.asJson)
        } yield res
      }

    //localhost:9001/employees/add_to_archive/ca19c82f-1753-42ad-b2c5-e32957a0626b
    case PUT -> Root / "add_to_archive" / UUIDVar(id) =>
      for {
        ans <- employeeService.addToArchive(id)
        res <- Ok(ans.asJson)
      } yield res

    case PUT -> Root / "unarchive" / UUIDVar(id) =>
      for {
        ans <- employeeService.unarchive(id)
        res <- Ok(ans.asJson)
      } yield res

  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )

}
