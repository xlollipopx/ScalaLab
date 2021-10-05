package tf

import cats.syntax.all.*
import cats.effect.{ContextShift, IO}
import cats.implicits.*
import eu.timepit.refined.api.Refined
import org.mockito.MockitoSugar
import eu.timepit.refined.{refineMV, refineV, W}
import org.mockito.MockitoSugar.mock
import org.scalatest
import org.mockito.Mockito
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import tf.domain.employee.{Employee, EmployeeDto, Name, Position}
import tf.services.EmployeeService
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.string.MatchesRegex
import org.mockito.IdiomaticMockito
import tf.domain.money.Money
import tf.validation.EmployeeValidationError.{InvalidDate, InvalidMoneyFormat, InvalidName, InvalidPosition}
import tf.validation.{EmployeeValidationError, EmployeeValidator}

import java.time.LocalDate
import java.util.Currency
import scala.concurrent.ExecutionContext
class TaglessFinalSpec extends AnyFreeSpec with Matchers with MockitoSugar {

  val INPUT_ONE             = "create 2000-03-11 Tony Gates 2000 USD Boss"
  val INPUT_TWO             = "create 1900-04-18"
  val mockedEmployeeService = mock[EmployeeService[IO]]

  val mockedEmployeeValidator = mock[EmployeeValidator]

  val date = LocalDate.parse("2000-03-11")
  val firstNameOne:  Name     = "Tony"
  val secondNameOne: Name     = "Gates"
  val money:         Money    = Money(BigDecimal("2000"), Currency.getInstance("USD"))
  val position:      Position = "Boss"
  val tuple = (date, firstNameOne, secondNameOne, money, position)
  when(mockedEmployeeValidator.validateBirthDay("2000-03-11"))
    .thenReturn(Either.cond(date.isInstanceOf[LocalDate], date, InvalidDate))
  when(mockedEmployeeValidator.validateFirstName("Tony"))
    .thenReturn(Either.cond(firstNameOne.isInstanceOf[Name], firstNameOne, InvalidName))
  when(mockedEmployeeValidator.validateLastName("Gates"))
    .thenReturn(Either.cond(secondNameOne.isInstanceOf[Name], secondNameOne, InvalidName))
  when(mockedEmployeeValidator.validateMoney("2000", "USD"))
    .thenReturn(Either.cond(money.isInstanceOf[Money], money, InvalidMoneyFormat))
  when(mockedEmployeeValidator.validatePosition("Boss"))
    .thenReturn(Either.cond(position.isInstanceOf[Position], position, InvalidPosition))
  when(mockedEmployeeValidator.validate(EmployeeDto("2000-03-11", "Tony", "Gates", "2000", "USD", "Boss")))
    .thenReturn(
      Either.cond(tuple.isInstanceOf[(LocalDate, Name, Name, Money, Position)], tuple, InvalidPosition)
    )

  def assertIO[A](ioOne: IO[A], ioTwo: IO[A]): IO[scalatest.Assertion] = {
    for {
      one <- ioOne
      two <- ioTwo
    } yield (assert(one == two))
  }

  "employee service" - {

    "all" - {
      "all should return empty list" in {
        val actual = for {
          service <- EmployeeService.of[IO](new EmployeeValidator)
          list    <- service.all
        } yield list
        val expected = IO(List[Employee]())
        assertIO(actual, expected).unsafeRunSync()
      }

      "all should return list with one employee" in {
        val actual = for {
          service <- EmployeeService.of[IO](mockedEmployeeValidator)
          _       <- service.create(EmployeeDto("2000-03-11", "Tony", "Gates", "2000", "USD", "Boss"))
          list    <- service.all
        } yield list

        val expected = IO(
          List(
            Employee(
              1,
              date,
              firstNameOne,
              secondNameOne,
              Money(BigDecimal("2000"), Currency.getInstance("USD")),
              position
            )
          )
        )
        assertIO(actual, expected).unsafeRunSync()
      }
    }

    "create" - {
      "create should return valid employee" in {
        val actual = for {
          service  <- EmployeeService.of[IO](mockedEmployeeValidator)
          employee <- service.create(EmployeeDto("2000-03-11", "Tony", "Gates", "2000", "USD", "Boss"))
        } yield employee

        val expected = IO(
          Right(
            Employee(
              1,
              date,
              firstNameOne,
              secondNameOne,
              Money(BigDecimal("2000"), Currency.getInstance("USD")),
              position
            )
          )
        )
        assertIO(actual, expected).unsafeRunSync()
      }
    }

    "delete" - {
      for {
        service <- EmployeeService.of[IO](mockedEmployeeValidator)
        _       <- service.create(EmployeeDto("2000-03-11", "Tony", "Gates", "2000", "USD", "Boss"))
        _       <- service.delete(1)
        list    <- service.all
      } yield (assert(list.isEmpty))
    }

    "find" - {
      val actual = for {
        service  <- EmployeeService.of[IO](mockedEmployeeValidator)
        _        <- service.create(EmployeeDto("2000-03-11", "Tony", "Gates", "2000", "USD", "Boss"))
        employee <- service.find(1)
      } yield employee

      val expected = IO(
        Option(
          Employee(
            1,
            date,
            firstNameOne,
            secondNameOne,
            Money(BigDecimal("2000"), Currency.getInstance("USD")),
            position
          )
        )
      )
      assertIO(actual, expected).unsafeRunSync()
    }

  }

}
