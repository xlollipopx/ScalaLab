package tf

import cats.effect.{ExitCode, IO, IOApp}
import tf.routers.{EmployeeRouter}
import tf.services.console.EmployeeConsoleService
import tf.validation.EmployeeValidator

object EmployeeMain extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      service <- EmployeeConsoleService.of[IO](new EmployeeValidator())
      router   = routers.EmployeeRouter(service)
      _       <- ConsoleInterface(router).repl
    } yield ExitCode.Success
  }

}
