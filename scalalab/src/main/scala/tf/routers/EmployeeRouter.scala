package tf.routers

import cats.Monad
import cats.data.{Kleisli, OptionT}
import cats.syntax.all.*
import tf.domain.employee.EmployeeDto
import tf.effects.ToNumeric.ToNumeric
import tf.effects.ToNumeric.ToNumeric.ToNumericStringOps
import tf.services.EmployeeService

object EmployeeRouter {

  def apply[F[_]: Monad: ToNumeric](employeeService: EmployeeService[F]): Kleisli[OptionT[F, *], List[String], String] =
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
            id     <- employeeId.toLongF
            result <- employeeService.find(id)
          } yield result.toString
        }

      case "delete" :: employeeId :: _ =>
        OptionT.liftF {
          for {
            id     <- employeeId.toLongF
            result <- employeeService.delete(id)
          } yield result.toString
        }

      case "update" :: employeeId :: birthday :: firstName :: lastName :: salary :: currency :: position :: _ =>
        OptionT.liftF {
          for {
            id <- employeeId.toLongF
            result <- employeeService.update(
              id,
              EmployeeDto(birthday, firstName, lastName, salary, currency, position)
            )
          } yield result.toString
        }

      case "create" :: birthday :: firstName :: lastName :: salary :: currency :: position :: _ =>
        OptionT.liftF {
          for {
            result <- employeeService.create(EmployeeDto(birthday, firstName, lastName, salary, currency, position))
          } yield result.toString
        }

      case _ => OptionT.none
    }
}
