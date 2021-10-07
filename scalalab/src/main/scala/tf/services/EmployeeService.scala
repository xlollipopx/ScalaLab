package tf.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import tf.domain.employee.{Employee, EmployeeDto}
import tf.validation.{EmployeeValidationError, EmployeeValidator}

trait EmployeeService[F[_]] extends Service[F, Employee, EmployeeValidationError, EmployeeDto] {}

object EmployeeService {

  def of[F[_]: Sync](employeeValidator: EmployeeValidator): F[EmployeeService[F]] = for {
    counter   <- Ref.of[F, Long](0)
    employees <- Ref.of[F, List[Employee]](List.empty)

  } yield new InMemoryEmployeeService(counter, employees, employeeValidator)

  final private class InMemoryEmployeeService[F[_]: Sync](
    counter:           Ref[F, Long],
    employees:         Ref[F, List[Employee]],
    employeeValidator: EmployeeValidator
  ) extends EmployeeService[F] {
    override def all: F[List[Employee]] = employees.get

    override def create(employeeDto: EmployeeDto): F[Either[EmployeeValidationError, Employee]] = {
      employeeValidator
        .validate(employeeDto)
        .traverse { case (birthdayRes, firstNameRes, lastNameRes, salaryRes, positionRes) =>
          for {
            id      <- counter.updateAndGet(_ + 1)
            employee = Employee(id, birthdayRes, firstNameRes, lastNameRes, salaryRes, positionRes)
            _       <- employees.update(employee :: _)
          } yield employee
        }
    }

    override def update(id: Long, employeeDto: EmployeeDto): F[Either[EmployeeValidationError, Boolean]] = {
      employeeValidator
        .validate(employeeDto)
        .traverse { case (birthday, firstName, lastName, salary, position) =>
          employees.modify { list =>
            list.find(x => x.id == id) match {
              case Some(_) =>
                list
                  .filterNot(_.id == id)
                  .appended(Employee(id, birthday, firstName, lastName, salary, position)) -> true
              case None => list -> false
            }
          }
        }
    }

    override def find(id: Long): F[Option[Employee]] = employees.get.map(l => l.find(_.id == id))

    override def delete(id: Long): F[Boolean] = employees.modify { list =>
      list.filter(_.id != id) -> !list.forall(_.id != id)
    }

  }
}
