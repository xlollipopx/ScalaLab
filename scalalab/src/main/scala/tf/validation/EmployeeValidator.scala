package tf.validation

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxApply, catsSyntaxTuple2Semigroupal, catsSyntaxValidatedIdBinCompat0}
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.{refineMV, refineV}
import tf.domain.employee.{Employee, Name, Position}
import tf.domain.money.Money
import tf.validation.EmployeeValidator.EmployeeValidationError.{
  InvalidDate,
  InvalidPosition,
  NameIsInvalid,
  WrongMoneyFormat
}
import cats.implicits._

import java.time.{Instant, LocalDate}
import java.time.format.DateTimeFormatter
import java.util.{Currency, UUID}
import scala.util.{Failure, Success, Try}

object EmployeeValidator {

  sealed trait EmployeeValidationError
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  object EmployeeValidationError {
    final case object NameIsInvalid extends EmployeeValidationError {
      override def toString: String = "Must contain alphabetic first name and last name!"
    }

    final case object WrongMoneyFormat extends EmployeeValidationError {
      override def toString: String = "Wrong money format!"
    }

    final case object InvalidDate extends EmployeeValidationError {
      override def toString: String = "Invalid date!"
    }
    final case object InvalidPosition extends EmployeeValidationError {
      override def toString: String = "Position must be alphabetic!"
    }

  }

  def validate(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    currency:  String,
    position:  String
  ) = for {
    birthdayRes <- Either.cond(
      LocalDate.parse(birthday).isInstanceOf[LocalDate],
      LocalDate.parse(birthday),
      InvalidDate
    )
    firstNameRes <- Either.cond(firstName.matches("^(([A-Za-z]+ ?){1,3})$"), firstName, NameIsInvalid)
    lastNameRes  <- Either.cond(lastName.matches("^(([A-Za-z]+ ?){1,3})$"), lastName, NameIsInvalid)
    salaryRes    <- validateMoney(salary, currency)
    positionRes  <- Either.cond(position.matches("[a-zA-Z]+"), position, InvalidPosition)
  } yield (birthdayRes, firstNameRes, lastNameRes, salaryRes, positionRes)

  def validateMoney(salary: String, currency: String): Either[EmployeeValidationError, Money] = {
    for {
      amount <- Either.cond(
        BigDecimal(salary).isInstanceOf[BigDecimal],
        BigDecimal(salary),
        WrongMoneyFormat
      )
      currency <- Either.cond(
        Currency.getInstance(currency).isInstanceOf[Currency],
        Currency.getInstance(currency),
        WrongMoneyFormat
      )
    } yield Money(amount, currency)
  }

}
