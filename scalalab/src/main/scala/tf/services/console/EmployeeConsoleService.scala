package tf.services.console

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import tf.domain.employee.Employee
import tf.dto.employee.EmployeeDto
import tf.util.ModelMapper.employeeDomainToDto
import tf.validation.{EmployeeValidationError, EmployeeValidator}

import java.util.UUID

trait EmployeeConsoleService[F[_]] extends Service[F, Employee, EmployeeValidationError, EmployeeDto] {}

object EmployeeConsoleService {

  def of[F[_]: Sync](employeeValidator: EmployeeValidator): F[EmployeeConsoleService[F]] = for {
    counter   <- Ref.of[F, Long](0)
    employees <- Ref.of[F, List[Employee]](List.empty)

  } yield new InMemoryEmployeeService(counter, employees, employeeValidator)

  final private class InMemoryEmployeeService[F[_]: Sync](
    counter:           Ref[F, Long],
    employees:         Ref[F, List[Employee]],
    employeeValidator: EmployeeValidator
  ) extends EmployeeConsoleService[F] {
    override def all: F[List[EmployeeDto]] = employees.get.map(l => l.map(employeeDomainToDto))

    override def create(employeeDto: EmployeeDto): F[Either[EmployeeValidationError, EmployeeDto]] = {
      employeeValidator
        .validate(employeeDto)
        .traverse { case (birthdayRes, firstNameRes, lastNameRes, salaryRes, positionRes) =>
          for {
            id <- counter.updateAndGet(_ + 1)
            employee = Employee(
              UUID.randomUUID(),
              birthdayRes,
              firstNameRes,
              lastNameRes,
              salaryRes,
              positionRes,
              employeeDto.is_archived
            )
            _ <- employees.update(employee :: _)
          } yield employeeDto
        }
    }

    override def update(id: UUID, employeeDto: EmployeeDto): F[Either[EmployeeValidationError, Boolean]] = {
      employeeValidator
        .validate(employeeDto)
        .traverse { case (birthday, firstName, lastName, salary, position) =>
          employees.modify { list =>
            list.find(x => x.id == id) match {
              case Some(_) =>
                list
                  .filterNot(_.id == id)
                  .appended(
                    Employee(id, birthday, firstName, lastName, salary, position, employeeDto.is_archived)
                  ) -> true
              case None => list -> false
            }
          }
        }
    }

    override def find(id: UUID): F[Option[EmployeeDto]] =
      employees.get.map(l =>
        l.find(_.id == id) match {
          case Some(x) => Option(employeeDomainToDto(x))
          case None    => None
        }
      )

    override def delete(id: UUID): F[Boolean] = employees.modify { list =>
      list.filter(_.id != id) -> !list.forall(_.id != id)
    }

  }
}
