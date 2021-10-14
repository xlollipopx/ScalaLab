package tf
import cats.effect.{Async, ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Resource, Sync, Timer}
import doobie.ConnectionIO
import doobie.util.fragment.Fragment
import doobie.util.transactor
import org.http4s.server.blaze.BlazeServerBuilder
import tf.db.DbCommon.createTableEmployeesSql
import tf.db.DbTransactor
import tf.modules.HttpApi
import doobie.implicits._
import tf.domain.employee.Employee
import tf.dto.employee.EmployeeDto
import tf.repository.EmployeeRepository
import tf.repository.impl.doobie.meta._
import tf.services.http.HttpEmployeeService
import org.http4s._
import org.http4s.server.Server
import tf.validation.EmployeeValidator

import scala.concurrent.ExecutionContext

object EmployeeHttpMain extends IOApp {

  def setUp[F[_]: ContextShift: Async](): Resource[F, HttpApp[F]] = for {
    transactor <- DbTransactor.make[F]
    repository  = EmployeeRepository.of[F](transactor)
    service     = HttpEmployeeService.of[F](repository, new EmployeeValidator)

    httpApp = HttpApi.make[F](service).httpApp
  } yield httpApp

  override def run(args: List[String]): IO[ExitCode] = serverResource[IO]
    .use(_ => IO.never)
    .as(ExitCode.Success)

  private def serverResource[F[_]: ContextShift: ConcurrentEffect: Timer]: Resource[F, Server[F]] = for {

    httpApp <- setUp[F]()

    server <- BlazeServerBuilder[F](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .resource

  } yield server

}
