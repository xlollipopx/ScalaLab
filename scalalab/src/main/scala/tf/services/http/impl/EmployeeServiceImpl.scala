package tf.services.http.impl

import cats.Monad
import cats.effect.Sync
import tf.domain.employee
import cats.data.{EitherT, OptionT}
import cats.implicits._
import tf.domain.employee.Employee
import tf.dto.employee.EmployeeDto
import tf.repository.EmployeeRepository
import tf.repository.impl.doobie.DoobieEmployeeRepository
import tf.services.http.HttpEmployeeService
import tf.util.ModelMapper.employeeDomainToDto
import tf.validation.EmployeeValidationError
import tf.validation.EmployeeValidationError.EmployeeNotFound
import tf.validation.EmployeeValidator

import java.util.UUID

class EmployeeServiceImpl[F[_]: Sync: Monad](employeeRepository: EmployeeRepository[F], validator: EmployeeValidator)
  extends HttpEmployeeService[F] {
  override def all: F[List[EmployeeDto]] = for {
    employees <- employeeRepository.all
  } yield employees.map(employeeDomainToDto)

  override def create(item: EmployeeDto): F[Either[EmployeeValidationError, EmployeeDto]] = {
    val result: EitherT[F, EmployeeValidationError, EmployeeDto] = for {

      x                              <- EitherT(validator.validate(item).pure[F])
      (date, fn, ln, money, position) = x
      domain                          = Employee(UUID.randomUUID(), date, fn, ln, money, position, item.is_archived)
      _                               = println(item.is_archived)
      _                              <- EitherT.liftF(employeeRepository.create(domain))
      //employeeWithPk                  = domain.copy()
    } yield employeeDomainToDto(domain)

    result.value
  }

  override def update(id: UUID, item: EmployeeDto): F[Either[EmployeeValidationError, EmployeeDto]] = {
    val result: EitherT[F, EmployeeValidationError, EmployeeDto] = for {
      _                              <- EitherT.fromOptionF(employeeRepository.findById(id), EmployeeNotFound)
      x                              <- EitherT(validator.validate(item).pure[F])
      (date, fn, ln, money, position) = x
      domain                          = Employee(id, date, fn, ln, money, position, item.is_archived)

      _ <- EitherT.liftF(employeeRepository.update(id, domain))
    } yield employeeDomainToDto(domain)
    result.value
  }

  override def find(id: UUID): F[Option[EmployeeDto]] = {
    val result: OptionT[F, EmployeeDto] = for {
      employee <- OptionT(employeeRepository.findById(id))
    } yield employeeDomainToDto(employee)
    result.value
  }

  override def delete(id: UUID): F[Boolean] =
    for {
      cnt <- (employeeRepository.delete(id))
      _    = print(cnt)
      res  = if (cnt == 1) true else false
    } yield res

  override def addToArchive(id: UUID): F[Int] = queryInt(id, employeeRepository.addToArchive)

  override def unarchive(id: UUID): F[Int] = queryInt(id, employeeRepository.unarchive)

  private def queryInt(id: UUID, f: UUID => F[Int]): F[Int] =
    for {
      res <- f(id)
    } yield res
}
