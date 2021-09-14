package errors

import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.implicits._
import errors.Errors.AccountValidationError.NameIsInvalid
import errors.Errors.AccountValidator

import java.time.{Instant, LocalDate, Year}
import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.all.Day
import shapeless.Witness

import java.util.Calendar


object Errors {


  type SecurityCode = String Refined MatchesRegex[W.`"^[0-9]{3}$"`.T]
  type CardNumber = String Refined MatchesRegex[W.`"^[0-9]{16}$"`.T]
  type Owner = String Refined MatchesRegex[W.`"^(([A-Za-z]+ ?){1,3})$"`.T]
  type Age =  Int Refined Interval.ClosedOpen[18, 150]
  type PassportNumber = String Refined MatchesRegex[W.`"^(?=.*[A-Z])(?=.*[0-9])[A-Z0-9]+$"`.T]
  type Day = Int
  type Month = Int
  type Year = Int

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


    def validateParameter[T, P](
                                 parameter: T,
                                 error: AccountValidationError
                               )(
                                 implicit v: Validate[T, P]
                               ): AllErrorsOr[Refined[T, P]] = {
      val res: Either[String, Refined[T, P]] = refineV(parameter)
      res.left.map(_ => error).toValidatedNec
    }


    def validateOwner(name: String): AllErrorsOr[Owner] = {
      validateParameter(name, NameIsInvalid)
    }

    def validateSecurityCode(rawSecurityCode: String): AllErrorsOr[SecurityCode] = {
      validateParameter(rawSecurityCode, CvvIsNotNumeric)
    }

    def validateCardCvv(cvv: String): AllErrorsOr[SecurityCode] = {
      validateParameter(cvv, CvvIsNotNumeric)
    }

    def validateCardNumber(cardNumber: String): AllErrorsOr[CardNumber] = {
      validateParameter(cardNumber, CardNumberIsNotNumeric)
    }

    def validatePersonAge(age: Int): AllErrorsOr[Age] = {
      validateParameter(age, AgeIsOutOfBounds)
    }



    def validateBirthDate(day: Int, month: Int, year: Int): AllErrorsOr[Date] = {
      def validateDay: AllErrorsOr[Day] = {
        day match {
          case day if day < 1
            || day > 31 => DateIsNotValid.invalidNec
          case _ => day.validNec
        }
      }
      def validateMonth: AllErrorsOr[Month] = {
        day match {
          case month if month < 1
            || month > 12 => DateIsNotValid.invalidNec
          case _ => month.validNec
        }
      }
      def validateYear: AllErrorsOr[Year] = {
        year match {
          case year if year > 1950 && year < Year.now.getValue => year.validNec
          case _ => DateIsNotValid.invalidNec
        }
      }
      (validateDay, validateMonth, validateYear).mapN(Date)
    }

    def validateCardDate(day: Int, month: Int, year: Int): AllErrorsOr[Date] = {
      def validateDay: AllErrorsOr[Day] = {
        day match {
          case day if day < Calendar.getInstance().get(Calendar.DAY_OF_MONTH)
            || day > 31 => DateIsNotValid.invalidNec
          case _ => day.validNec
        }
      }
      def validateMonth: AllErrorsOr[Month] = {
        day match {
          case month if month < Calendar.getInstance().get(Calendar.MONTH)
            || month > 31 => DateIsNotValid.invalidNec
          case _ => month.validNec
        }
      }
      def validateYear: AllErrorsOr[Year] = {
        year match {
          case year if year > Year.now.getValue => year.validNec
          case _ => DateIsNotValid.invalidNec
        }
      }
      (validateDay, validateMonth, validateYear).mapN(Date)
    }


    def validatePassportNumber(passportNumber: String): AllErrorsOr[PassportNumber] = {
      validateParameter(passportNumber, DateIsNotValid)
    }


    def validatePerson(person: PersonDto): AllErrorsOr[Person] = (
      validateOwner(person.name),
      validatePersonAge(person.age),
      validatePassportNumber(person.passportNumber),
      validateBirthDate(person.day, person.month, person.year)
      ).mapN(Person)

    def validateCard(paymentCard: PaymentCardDto): AllErrorsOr[PaymentCard] = (
      validateCardNumber(paymentCard.cardNumber),
      validateBirthDate(paymentCard.day, paymentCard.month, paymentCard.year),
      validateCardCvv(paymentCard.cvv)
      ).mapN(PaymentCard)

    def validate(personDto: PersonDto, paymentCardDto: PaymentCardDto): AllErrorsOr[Account] = (
      validatePerson(PersonDto(personDto.name, personDto.age, personDto.passportNumber,
        personDto.day, personDto.month, personDto.year)),
      validateCard(PaymentCardDto(paymentCardDto.cardNumber, paymentCardDto.day,
        paymentCardDto.month, paymentCardDto.year, paymentCardDto.cvv))
      ).mapN(Account)

  }


  def main(args: Array[String]): Unit = {

    import AccountValidator._

    println(validate(PersonDto("Din Smith", 20, "MP142293", 1,5,2001),
      PaymentCardDto("3948576857492837", 20, 1, 2024, "810")))


    println(validate(PersonDto("Din", 16, "MP142293", 1,5,2001),
      PaymentCardDto("19485768592837", 20, 1, 1900, "14")))

   // println(foo("sflsvm", NameIsInvalid))

  }


}
