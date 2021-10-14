package tf.repository.impl.doobie

import doobie.implicits._
import cats.Functor
import cats.effect.Bracket
import doobie.{Fragment, Transactor}
import tf.domain.employee.Employee
import tf.repository.EmployeeRepository
import meta.implicits._
import doobie.postgres.implicits._
import java.util.UUID

class DoobieEmployeeRepository[F[_]: Functor: Bracket[*[_], Throwable]](
  tx: Transactor[F]
) extends EmployeeRepository[F] {
  private val selectEmployee: Fragment = fr"SELECT * FROM employees"
  private val createEmployee: Fragment = sql"INSERT INTO employees(" ++
    sql"id, birthday, firstName, lastName, salary, position, is_archived)"
  private val updateEmployee: Fragment = fr"UPDATE employees"
  private val deleteEmployee: Fragment = fr"DELETE FROM employees"

  override def all: F[List[Employee]] = selectEmployee.query[Employee].to[List].transact(tx)

  override def create(item: Employee): F[Int] =
    (createEmployee ++ sql"VALUES(" ++
      sql"${item.id}, ${item.birthday}, ${item.firstName}, " ++
      sql"${item.lastName}, ${item.salary}, ${item.position}, ${item.is_archived})").update.run.transact(tx)

  override def update(id: UUID, item: Employee): F[Int] = {
    (updateEmployee ++
      fr"SET birthday = ${item.birthday}, " ++
      fr"firstName = ${item.firstName}, " ++
      fr"lastName = ${item.lastName}, " ++
      fr"salary = ${item.salary}, " ++
      fr"position = ${item.position} " ++
      fr"is_archived = ${item.is_archived}" ++
      fr"WHERE id = ${item.id}").update.run.transact(tx)
  }

  override def findById(id: UUID): F[Option[Employee]] = (selectEmployee ++ fr"WHERE id = $id")
    .query[Employee]
    .option
    .transact(tx)

  override def delete(id: UUID): F[Int] = {
    (deleteEmployee ++ fr"WHERE id = $id").update.run.transact(tx)
  }
  //ca19c82f-1753-42ad-b2c5-e32957a0626b
  override def addToArchive(id: UUID): F[Int] = {
    (updateEmployee ++ fr"SET is_archived = true WHERE id = ${id}").update.run.transact(tx)
  }
  override def unarchive(id: UUID): F[Int] = {
    (updateEmployee ++ fr"SET is_archived = false WHERE id = ${id}").update.run.transact(tx)
  }

}
