package tf.service

import cats.effect.Sync
import cats.implicits._
import cats.effect.Ref
import tf.domain.employee.Employee
import tf.domain.money
import tf.validation.EmployeeValidator
import tf.validation.EmployeeValidator.EmployeeValidationError

import java.time.LocalDate

trait EmployeeService[F[_]] extends Service[F, Employee, EmployeeValidationError] {}

object EmployeeService {
  def of[F[_]: Sync]: F[EmployeeService[F]] = for {
    counter   <- Ref.of[F, Long](0)
    employees <- Ref.of[F, Map[Long, Employee]](Map.empty)
  } yield new InMemoryEmployeeService(counter, employees)

  final private class InMemoryEmployeeService[F[_]: Sync](
    counter:   Ref[F, Long],
    employees: Ref[F, Map[Long, Employee]]
  ) extends EmployeeService[F] {

    override def all: F[Map[Long, Employee]] = employees.get

    override def create(
      birthday:  String,
      firstName: String,
      lastName:  String,
      salary:    String,
      currency:  String,
      position:  String
    ): F[Either[EmployeeValidationError, Employee]] =
      EmployeeValidator.validate(birthday, firstName, lastName, salary, currency, position).traverse {
        case (birthday, firstName, lastName, salary, position) =>
          for {
            id      <- counter.updateAndGet(_ + 1)
            employee = Employee(id, birthday, firstName, lastName, salary, position)
            _       <- employees.update(_.updated(id, employee))
          } yield employee
      }

    override def update(employee: Employee): F[Either[EmployeeValidationError, Boolean]] =
      EmployeeValidator
        .validate(
          employee.birthday.toString,
          employee.firstName,
          employee.lastName,
          employee.salary.amount.toString,
          employee.salary.currency.toString,
          employee.position
        )
        .traverse { _ =>
          employees.modify { employees =>
            if (employees.contains(employee.id)) employees.updated(employee.id, employee) -> true
            else employees                                                                -> false
          }
        }

    override def find(id: Long): F[Option[Employee]] = employees.get.map(l => l.get(id))

    override def delete(id: Long): F[Boolean] =
      employees.modify { employees =>
        employees.removed(id) -> employees.contains(id)
      }
  }

}
