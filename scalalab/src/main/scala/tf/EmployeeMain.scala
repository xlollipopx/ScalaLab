package tf

import cats.effect.{Async, ExitCode, IO, IOApp, Sync}
import tf.service.EmployeeService
import cats.implicits._

object EmployeeMain extends IOApp {

  def program[F[_]: Sync]: F[Unit] = for {
    service <- EmployeeService.of[F]
    _       <- service.create("2001-05-25", "Adam", "Lambert", "2342", "USD", "chief")
    _       <- service.create("2000-05-25", "Jhon", "Cage", "2442", "USD", "chief")
    res1    <- service.all
    res2    <- service.find(1)
  } yield {
    println(res1)
    println(res2)
  }

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- program[IO]
  } yield ExitCode.Success

}
