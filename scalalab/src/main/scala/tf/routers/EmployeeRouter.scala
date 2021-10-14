package tf.routers

import cats.Monad
import cats.data.{Kleisli, OptionT}
import tf.dto.employee.EmployeeDto
import tf.effects.ToNumeric.ToNumeric
import tf.services.console.EmployeeConsoleService
import cats.implicits._
import java.util.UUID

object EmployeeRouter {

  def apply[F[_]: Monad: ToNumeric](
    employeeService: EmployeeConsoleService[F]
  ): Kleisli[OptionT[F, *], List[String], String] =
    Kleisli[OptionT[F, *], List[String], String] {
      case "all" :: _ =>
        OptionT.liftF {
          for {
            all <- employeeService.all
          } yield all.toString()
        }

      case "find" :: employeeId :: _ =>
        OptionT.liftF {
          for {
            id     <- UUID.fromString(employeeId).pure[F]
            result <- employeeService.find(id)
          } yield result.toString
        }

      case "delete" :: employeeId :: _ =>
        OptionT.liftF {
          for {
            id     <- UUID.fromString(employeeId).pure[F]
            result <- employeeService.delete(id)
          } yield result.toString
        }

      case "update" :: employeeId :: birthday :: firstName :: lastName :: salary :: currency :: position :: is_archived :: _ =>
        OptionT.liftF {
          for {
            id <- UUID.fromString(employeeId).pure[F]

            result <- employeeService.update(
              id,
              EmployeeDto(birthday, firstName, lastName, salary, currency, position, is_archived.toBoolean)
            )
          } yield result.toString
        }

      case "create" :: birthday :: firstName :: lastName :: salary :: currency :: position :: is_archived :: _ =>
        OptionT.liftF {
          for {
            result <- employeeService.create(
              EmployeeDto(birthday, firstName, lastName, salary, currency, position, is_archived.toBoolean)
            )
          } yield result.toString
        }

      case _ => OptionT.none
    }
}
