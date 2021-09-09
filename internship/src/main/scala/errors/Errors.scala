package errors

import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.implicits._
import java.time.Year


object Errors {

  final case class Account(person: Person, card: PaymentCard)

  final case class Person(name: String, age: Int, passportNumber: String, birthDate: Date)

  final case class Date(day: Int, month: Int, year: Int)

  final case class PaymentCard(cardNumber: String, expirationDate: Date, cvv: Short)

  sealed trait AccountValidationError

  object AccountValidationError {

    final case object UsernameLengthIsInvalid extends AccountValidationError {
      override def toString: String = "Username must be between 3 and 30 characters"
    }

    final case object UsernameHasSpecialCharacters extends AccountValidationError {
      override def toString: String = "Username cannot contain special characters"
    }

    final case object CvvIsNotNumeric extends AccountValidationError {
      override def toString: String = "Cvv number must be 3 digit number"
    }

    final case object CardNumberIsNotNumeric extends AccountValidationError {
      override def toString: String = "Card number must 16 digit number"
    }

    final case object DateIsNotValid extends AccountValidationError {
      override def toString: String = "Impossible date"
    }

    final case object ImpossiblePassportNumber extends AccountValidationError {
      override def toString: String = "Impossible passport number"
    }

    final case object AgeIsNotNumeric extends AccountValidationError {
      override def toString: String = "Age must be a number"
    }

    final case object AgeIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Person must be older then 18"
    }

  }

  def parseIntEither(string: String): Either[String, Int] =
    string.toIntOption.toRight(s"$string doesn't contain an integer")

  object AccountValidator {

    import AccountValidationError._

    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    def validateName(name: String): AllErrorsOr[String] = {

      def validateUsernameLength: AllErrorsOr[String] =
        if (name.length >= 3 && name.length <= 30) name.validNec
        else UsernameLengthIsInvalid.invalidNec

      def validateUsernameContents: AllErrorsOr[String] =
        if (name.matches("^[a-zA-Z]+$")) name.validNec
        else UsernameHasSpecialCharacters.invalidNec

      validateUsernameLength *> validateUsernameContents
    }

    def validateCardCvv(cvv: String): AllErrorsOr[Short] =
      if (cvv.matches("[0-9]{3}")) cvv.toShort.validNec
      else CvvIsNotNumeric.invalidNec

    def validateCardNumber(cardNumber: String): AllErrorsOr[String] =
      if (cardNumber.matches("[0-9]{16}")) cardNumber.validNec
      else CardNumberIsNotNumeric.invalidNec

    def validatePersonAge(age: String): AllErrorsOr[Int] =
      parseIntEither(age).
        left.map(_ => AgeIsNotNumeric).toValidatedNec

    def validateDate(date: Date): AllErrorsOr[Date] = date match {
      case Date(d, m, y) if d > 31 || d < 1 => DateIsNotValid.invalidNec
      case Date(d, m, y) if m > 12 || d < 1 => DateIsNotValid.invalidNec
      case Date(d, m, y) if y < Year.now.getValue - 100 => DateIsNotValid.invalidNec
      case _ => date.validNec
    }

    def validatePassportNumber(passportNumber: String): AllErrorsOr[String] =
      if(passportNumber.matches("^(?=.*[A-Z])(?=.*[0-9])[A-Z0-9]+$")) passportNumber.validNec
      else ImpossiblePassportNumber.invalidNec


    def validatePerson(name: String, age: String, passportNumber: String, birthDay: Date): AllErrorsOr[Person] = (
     validateName(name),
      validatePersonAge(age),
      validatePassportNumber(passportNumber),
      validateDate(birthDay)
      ).mapN(Person)

    def validateCard(cardNumber: String, expirationDate: Date, cvv: String): AllErrorsOr[PaymentCard] = (
      validateCardNumber(cardNumber),
      validateDate(expirationDate),
      validateCardCvv(cvv)
    ).mapN(PaymentCard)

    def validate(name: String, age: String, passportNumber: String, birthDay: Date,
                 cardNumber: String, expirationDate: Date, cvv: String): AllErrorsOr[Account] = (
      validatePerson(name, age, passportNumber, birthDay),
      validateCard(cardNumber, expirationDate, cvv)
    ).mapN(Account)

  }


  def main(args: Array[String]): Unit = {

    import AccountValidator._

    println(validate("Din", "20", "MP142293", Date(1,5,2001),
      "3948576857492837", Date(20, 1, 2024), "810"))

    println(validate("Din", "20", "MP142293", Date(1,5,2001),
      "19485768592837", Date(20, 1, 2024), "14"))

    println(validate("Din", "2d0", "MP142293", Date(1,5,2001),
      "19485768592837", Date(20, 1, 1900), "14"))


  }


}
