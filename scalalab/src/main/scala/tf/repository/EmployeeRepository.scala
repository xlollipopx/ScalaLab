package tf.repository

import cats.effect.Sync
import doobie.Transactor
import tf.domain.employee.Employee
import tf.repository.impl.doobie.DoobieEmployeeRepository
import tf.validation.EmployeeValidationError

import java.util.UUID

trait EmployeeRepository[F[_]] {
  def all: F[List[Employee]]
  def create(item:     Employee): F[Int]
  def update(id:       UUID, item: Employee): F[Int]
  def findById(id:     UUID): F[Option[Employee]]
  def delete(id:       UUID): F[Int]
  def addToArchive(id: UUID): F[Int]
  def unarchive(id:    UUID): F[Int]
}

object EmployeeRepository {
  def of[F[_]: Sync](tx: Transactor[F]): DoobieEmployeeRepository[F] =
    new DoobieEmployeeRepository[F](tx)
}
