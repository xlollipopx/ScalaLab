package errors

import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.implicits._

import java.time.Year
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string.MatchesRegex


object Errors {

  type SecurityCode = String Refined MatchesRegex[W.`"^[0-9]{3}$"`.T]
  type CardNumber = String Refined MatchesRegex[W.`"^[0-9]{16}$"`.T]
  type Owner = String Refined MatchesRegex[W.`"^(([A-Za-z]+ ?){1,3})$"`.T]
  type Age =  Int Refined Interval.ClosedOpen[18, 150]
  type PassportNumber = String Refined MatchesRegex[W.`"^(?=.*[A-Z])(?=.*[0-9])[A-Z0-9]+$"`.T]
  type Day = Int Refined Interval.ClosedOpen[1, 31]
  type Month = Int Refined Interval.ClosedOpen[1, 12]
  type Year = Int Refined Interval.ClosedOpen[1930 , 2040]


  final case class AccountDto(person: Person, card: PaymentCard)

  final case class PersonDto(name: String, age: Int, passportNumber: String, day: Int, month: Int, year: Int)

  final case class DateDto(day: Int, month: Int, year: Int)

  final case class PaymentCardDto(cardNumber: String, day: Int, month: Int, year: Int, cvv: String)



  final case class Account(person: Person, card: PaymentCard)

  final case class Person(name: Owner, age: Age, passportNumber: PassportNumber, birthDate: Date)

  final case class Date(day: Day, month: Month, year: Year)

  final case class PaymentCard(cardNumber: CardNumber, expirationDate: Date, cvv: SecurityCode)


  sealed trait AccountValidationError

  object AccountValidationError {

    final case object NameIsInvalid extends AccountValidationError {
      override def toString: String = "Must contain alphabetic first name and last name"
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

  object AccountValidator {

    import AccountValidationError._

    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    def validateUser(name: String): AllErrorsOr[Owner] = {
        val res: Either[String, Owner] = refineV(name)
        res.left.map(_ => NameIsInvalid).toValidatedNec
    }

    def validateCardCvv(cvv: String): AllErrorsOr[SecurityCode] = {
      val res: Either[String, SecurityCode] = refineV(cvv)
      res.left.map(_ => CvvIsNotNumeric).toValidatedNec
    }


    def validateCardNumber(cardNumber: String): AllErrorsOr[CardNumber] = {
      val res: Either[String, CardNumber] = refineV(cardNumber)
      res.left.map(_ => CardNumberIsNotNumeric).toValidatedNec
    }

    def validatePersonAge(age: Int): AllErrorsOr[Age] = {
      val res: Either[String, Age] = refineV(age)
      res.left.map(_ => AgeIsOutOfBounds).toValidatedNec
    }


    def validateDate(day: Int, month: Int, year: Int): AllErrorsOr[Date] = {
      def validateDay: AllErrorsOr[Day] = {
        val res: Either[String, Day] = refineV(day)
        res.left.map(_ => DateIsNotValid).toValidatedNec
      }
      def validateMonth: AllErrorsOr[Month] = {
        val res: Either[String, Month] = refineV(month)
        res.left.map(_ => DateIsNotValid).toValidatedNec
      }
      def validateYear: AllErrorsOr[Year] = {
        val res: Either[String, Year] = refineV(year)
        res.left.map(_ => DateIsNotValid).toValidatedNec
      }

      (validateDay, validateMonth, validateYear).mapN(Date)
    }

    def validatePassportNumber(passportNumber: String): AllErrorsOr[PassportNumber] = {
      val res: Either[String, PassportNumber] = refineV(passportNumber)
      res.left.map(_ => ImpossiblePassportNumber).toValidatedNec
    }


    def validatePerson(name: String, age: Int, passportNumber: String, day: Int, month: Int, year: Int ): AllErrorsOr[Person] = (
     validateUser(name),
      validatePersonAge(age),
      validatePassportNumber(passportNumber),
      validateDate(day, month, year)
      ).mapN(Person)

    def validateCard(cardNumber: String, day: Int, month: Int, year: Int, cvv: String): AllErrorsOr[PaymentCard] = (
      validateCardNumber(cardNumber),
      validateDate(day, month, year),
      validateCardCvv(cvv)
    ).mapN(PaymentCard)

    def validate(personDto: PersonDto, paymentCardDto: PaymentCardDto): AllErrorsOr[Account] = (
      validatePerson(personDto.name, personDto.age, personDto.passportNumber,
        personDto.day, personDto.month, personDto.year),
      validateCard(paymentCardDto.cardNumber, paymentCardDto.day,
        paymentCardDto.month, paymentCardDto.year, paymentCardDto.cvv)
    ).mapN(Account)

  }


  def main(args: Array[String]): Unit = {

    import AccountValidator._

    println(validate(PersonDto("Din Smith", 20, "MP142293", 1,5,2001),
      PaymentCardDto("3948576857492837", 20, 1, 2024, "810")))


    println(validate(PersonDto("Din", 16, "MP142293", 1,5,2001),
      PaymentCardDto("19485768592837", 20, 1, 1900, "14")))


  }


}
