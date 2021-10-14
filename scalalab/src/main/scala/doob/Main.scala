package doob

import cats.data.NonEmptyList
import cats.effect._
import cats.implicits._
import doob.DbCommon.{createTableEmployeesSql, edit, populateDataSql}
import doob.Main.deleteEmployees
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.h2._

import java.util.Currency

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = DbTransactor
    .make[IO]
    .use { xa =>
      for {
        // setup
        _ <- setup().transact(xa)

        // business part
        // _ <- fetchAuthorById(1).option.transact(xa).map(println)
        //_ <- delete(2).transact(xa).map(println)
        //_ <- delete(1)
        //          _ <- fetchAuthorById(UUID.randomUUID()).option.transact(xa).map(println)
        //          _ <- fetchHarryPotterBooks.to[List].transact(xa).map(_.foreach(println))
        //          _ <- fetchBooksByAuthors(NonEmptyList.of(authorOdersky, authorRowling))
        //            .to[List]
        //            .transact(xa)
        //            .map(_.foreach(println))
        //          _ <- fetchBooksByYear(1998).transact(xa).map(_.foreach(println))
        //          _ <- fetchBooksByYearRange(1997, 2001).transact(xa).map(_.foreach(println))
        //          _ <-
        //            (insertBook("Harry Potter and the Cursed Child - Parts I & II", authorRowling, Year.of(2016)) *>
        //              fetchBooksByAuthors(NonEmptyList.of(authorRowling)).to[List])
        //              .transact(xa)
        //              .map(_.foreach(println))
        //          _ <- updateYearOfBook(bookHPStone, Year.of(2003)).transact(xa)
      } yield ()
    }
    .as(ExitCode.Success)
  val ddl1 = Fragment.const(createTableEmployeesSql)
  val dml  = Fragment.const(populateDataSql)
  val ed   = Fragment.const(edit)
  def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
    } yield ()

  val employees: Fragment =
    fr"SELECT * FROM employees"
  val deleteEmployees: Fragment =
    fr"DELETE FROM employees"

  def fetchAuthorById(id: Long): doobie.Query0[EmployeeDto] =
    (employees ++ fr"WHERE id = $id").query[EmployeeDto]

  def delete(id: Long): doobie.ConnectionIO[Int] = {
    sql"DELETE FROM employees WHERE id = $id".update.run
  }

  final case class EmployeeDto(
    id:        String,
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    Money,
    position:  String
  )

  implicit val moneyMeta: Meta[Money] =
    Meta[String]
      .timap(s => toMoney(s))(g => g.num.toString() + " " + g.cur.toString)

  def toMoney(s: String): Money = {
    val list = s.split("\\s").toList
    Money(BigDecimal(list.head), Currency.getInstance(list.last))
  }

  final case class Money(num: BigDecimal, cur: Currency)
}
