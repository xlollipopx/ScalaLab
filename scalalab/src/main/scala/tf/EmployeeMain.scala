package tf

import cats.effect.{ExitCode, IO, IOApp}
import tf.routers.EmployeeRouter
import tf.services.EmployeeService
import tf.validation.EmployeeValidator

object EmployeeMain extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      service <- EmployeeService.of[IO](new EmployeeValidator())
      router   = EmployeeRouter(service)
      _       <- ConsoleInterface(router).repl
    } yield ExitCode.Success
  }

}
