package tf.services.http

import cats.effect.Sync
import tf.dto.employee.EmployeeDto
import tf.repository.EmployeeRepository
import tf.services.http.impl.EmployeeServiceImpl
import tf.validation.{EmployeeValidationError, EmployeeValidator}

import java.util.UUID

trait HttpEmployeeService[F[_]] {
  def all: F[List[EmployeeDto]]

  def create(item: EmployeeDto): F[Either[EmployeeValidationError, EmployeeDto]]

  def update(id: UUID, item: EmployeeDto): F[Either[EmployeeValidationError, EmployeeDto]]

  def find(id: UUID): F[Option[EmployeeDto]]

  def delete(id:       UUID): F[Boolean]
  def addToArchive(id: UUID): F[Int]
  def unarchive(id:    UUID): F[Int]
}

object HttpEmployeeService {
  def of[F[_]: Sync](
    employeeRepository: EmployeeRepository[F],
    employeeValidator:  EmployeeValidator
  ): HttpEmployeeService[F] =
    new EmployeeServiceImpl[F](employeeRepository, employeeValidator)
}
