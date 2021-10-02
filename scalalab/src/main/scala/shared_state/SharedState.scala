package shared_state

import cats.effect
import cats.effect.std.Semaphore
import cats.effect.{Async, Concurrent, ExitCode, IO, IOApp, Ref, Sync}
import cats.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object RefsExerciseTwo extends IOApp {

  def memoize[F[_], A](f: F[A])(implicit C: Sync[F]): F[F[A]] = for {
    res <- f
  } yield (res.pure[F])

  override def run(args: List[String]): IO[ExitCode] = {

    val successProgram = IO(println("Hey!")) >> IO(42)

    /*
     * Should print
     * Hey!
     * 42
     * 42
     * */

    val successResult: IO[Unit] = for {
      mem <- memoize(successProgram)
      x   <- mem
      _   <- IO(println(x))
      y   <- mem
      _   <- IO(println(y))
    } yield ()

    val errorProgram = IO {
      println("Gonna Boom!");
      throw new IllegalArgumentException("BOOM")
    }

    /*
     * Should print
     * Gonna Boom!
     * java.lang.IllegalArgumentException: BOOM
     */

    val failedResult: IO[Unit] = (for {
      mem <- memoize(errorProgram)
      x   <- mem
      _   <- IO(println(x))
      y   <- mem
      _   <- IO(println(y))
    } yield ()).handleErrorWith(e => IO(println(e)))

    successResult *>
      failedResult *>
      IO(ExitCode.Success)
  }
}
/*
 * Try to implement SerialRef which will semantically block on modify and wait until inner f is completed
 * Question: What will happen in case of a function `f` will never terminate inside update or modify?
 */

object IosCommon {
  val logger = Slf4jLogger.getLogger[IO]
}

object SerialRefExercise extends IOApp {

  import IosCommon.logger

  trait SerialRef[F[_], A] {

    def get: F[A]

    def modify[B](f: A => F[(A, B)]): F[B]

    def update(f: A => F[A]): F[Unit]
  }

  def of[F[_]: Concurrent, A](value: A): F[SerialRef[F, A]] = {
    for {
      s <- Semaphore[F](1)
      r <- Ref[F].of(value)
    } yield {
      new SerialRef[F, A] {

        def get: F[A] = r.get

        def modify[B](f: A => F[(A, B)]): F[B] = {

          val res = for {
            _       <- s.acquire
            value   <- r.get
            pair    <- f(value)
            (v, str) = pair
            _       <- r.set(v)
            _       <- s.release
          } yield str

          res
        }

        def update(f: A => F[A]): F[Unit] = {
          for {
            _      <- s.acquire
            value  <- r.get
            resVal <- f(value)
            _      <- r.update(_ => resVal)
            _      <- s.release
          } yield ()
        }
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    def modifyHelperIO(ref: SerialRef[IO, Int], duration: FiniteDuration, i: Int, s: String): IO[String] =
      logger.info(s"$s started") *> ref
        .modify(x => IO.sleep(duration) *> IO((x + i, s)))
        .flatTap(s => logger.info(s"$s finished"))

    for {
      ref <- SerialRefExercise.of[IO, Int](1)
      _ <- List(
        modifyHelperIO(ref, 3.second, 10, "first modify"),
        modifyHelperIO(ref, 4.second, 20, "second modify")
      ).parSequence.void
      value <- ref.get
      _     <- logger.info(s"ref value should be 31, $value")
    } yield (ExitCode.Success)
  }
}
