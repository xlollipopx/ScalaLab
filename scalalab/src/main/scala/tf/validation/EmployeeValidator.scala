package tf.validation

import eu.timepit.refined.refineV
import tf.domain.employee._
import tf.domain.money.Money
import tf.dto.employee.EmployeeDto
import tf.validation.EmployeeValidationError._
//import tf.validation.EmployeeValidator.EmployeeValidationError
//import tf.validation.EmployeeValidator.EmployeeValidationError.{InvalidDate, InvalidMoneyFormat, InvalidName, InvalidPosition}

import java.time.{Instant, LocalDate}
import java.time.format.DateTimeFormatter
import java.util.{Currency, UUID}

sealed trait EmployeeValidationError

object EmployeeValidationError {
  final case object InvalidName extends EmployeeValidationError {
    override def toString: String = "Must contain alphabetic first name and last name!"
  }

  final case object EmployeeNotFound extends EmployeeValidationError {
    override def toString: String = "Not found!"
  }

  final case object InvalidMoneyFormat extends EmployeeValidationError {
    override def toString: String = "Wrong money format!"
  }

  final case object InvalidDate extends EmployeeValidationError {
    override def toString: String = "Invalid date!"
  }
  final case object InvalidPosition extends EmployeeValidationError {
    override def toString: String = "Position must be alphabetic!"
  }
}

class EmployeeValidator {

  def validate(employeeDTO: EmployeeDto) = for {
    birthdayRes  <- validateBirthDay(employeeDTO.birthday)
    firstNameRes <- validateFirstName(employeeDTO.firstName)
    lastNameRes  <- validateLastName(employeeDTO.lastName)
    salaryRes    <- validateMoney(employeeDTO.salary, employeeDTO.currency)
    positionRes  <- validatePosition(employeeDTO.position)
  } yield (birthdayRes, firstNameRes, lastNameRes, salaryRes, positionRes)

  def validatePosition(position: String): Either[EmployeeValidationError, Position] = {
    val res: Either[String, Position] = refineV(position)
    res.left.map(_ => InvalidPosition)
  }

  def validateFirstName(firstName: String): Either[EmployeeValidationError, Name] = {
    val res: Either[String, Name] = refineV(firstName)
    res.left.map(_ => InvalidName)
  }

  def validateLastName(lastName: String): Either[EmployeeValidationError, Name] = {
    val res: Either[String, Name] = refineV(lastName)
    res.left.map(_ => InvalidName)
  }

  def validateBirthDay(birthday: String): Either[EmployeeValidationError, LocalDate] = {
    Either.cond(
      LocalDate.parse(birthday).isInstanceOf[LocalDate],
      LocalDate.parse(birthday),
      InvalidDate
    )
  }

  def validateMoney(salary: String, currency: String): Either[EmployeeValidationError, Money] = {
    for {
      amount <- Either.cond(
        BigDecimal(salary).isInstanceOf[BigDecimal],
        BigDecimal(salary),
        InvalidMoneyFormat
      )
      currency <- Either.cond(
        Currency.getInstance(currency).isInstanceOf[Currency],
        Currency.getInstance(currency),
        InvalidMoneyFormat
      )
    } yield Money(amount, currency)
  }

}
