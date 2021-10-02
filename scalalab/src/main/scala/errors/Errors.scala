package errors

import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.implicits._
import errors.Errors.AccountValidationError.NameIsInvalid
import errors.Errors.AccountValidator

import scala.util.Try
import java.time.{Instant, LocalDate, Year}
import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.all.Day
import shapeless.Witness
import java.time.chrono._

import java.time.chrono.ChronoLocalDate
import java.time.format.DateTimeFormatter
import java.util.Calendar
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Errors {

  type SecurityCode   = String Refined MatchesRegex[W.`"^[0-9]{3}$"`.T]
  type CardNumber     = String Refined MatchesRegex[W.`"^[0-9]{16}$"`.T]
  type Owner          = String Refined MatchesRegex[W.`"^(([A-Za-z]+ ?){1,3})$"`.T]
  type Age            = Int Refined Interval.ClosedOpen[18, 150]
  type PassportNumber = String Refined MatchesRegex[W.`"^(?=.*[A-Z])(?=.*[0-9])[A-Z0-9]+$"`.T]
  type Date           = String
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  final case class AccountDto(person: PersonDto, card: PaymentCardDto)

  final case class PersonDto(name: String, age: Int, passportNumber: String, date: String)

  final case class PaymentCardDto(cardNumber: String, date: String, cvv: String)

  final case class Account(person: Person, card: PaymentCard)

  final case class Person(name: Owner, age: Age, passportNumber: PassportNumber, birthDate: Date)

  final case class PaymentCard(cardNumber: CardNumber, expirationDate: Date, cvv: SecurityCode)

  sealed trait AccountValidationError

  object AccountValidationError {

    final case object NameIsInvalid extends AccountValidationError {
      override def toString: String = "Must contain alphabetic first name and last name"
    }

    final case object ImpossibleCvv extends AccountValidationError {
      override def toString: String = "Cvv number must be 3 digit number"
    }

    final case object InvalidCardNumber extends AccountValidationError {
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
      error:     AccountValidationError
    )(
      implicit v: Validate[T, P]
    ): AllErrorsOr[Refined[T, P]] = {
      val res: Either[String, Refined[T, P]] = refineV(parameter)
      res.left.map(_ => error).toValidatedNec
    }

    def validateOwner(name: String): AllErrorsOr[Owner] = {
      validateParameter(name, NameIsInvalid)
    }

    def validateCardCvv(cvv: String): AllErrorsOr[SecurityCode] = {
      validateParameter(cvv, ImpossibleCvv)
    }

    def validateCardNumber(cardNumber: String): AllErrorsOr[CardNumber] = {
      validateParameter(cardNumber, InvalidCardNumber)
    }

    def validatePersonAge(age: Int): AllErrorsOr[Age] = {
      validateParameter(age, AgeIsOutOfBounds)
    }

    def validateCardDate(rawDate: String): AllErrorsOr[Date] = {

      Try(LocalDate.parse(rawDate, formatter)) match {
        case Success(date) if date.isAfter(LocalDate.now)  => rawDate.validNec
        case Success(date) if date.isBefore(LocalDate.now) => DateIsNotValid.invalidNec
        case Failure(_)                                    => DateIsNotValid.invalidNec
      }
    }

    def validateBirthDate(rawDate: String): AllErrorsOr[Date] = {
      Try(LocalDate.parse(rawDate, formatter)) match {
        case Success(date)
            if date.isAfter(LocalDate.parse("1950-01-01"))
              && date.isBefore(LocalDate.now) =>
          rawDate.validNec
        case Failure(e) => DateIsNotValid.invalidNec
        case _          => DateIsNotValid.invalidNec
      }
    }

    def validatePassportNumber(passportNumber: String): AllErrorsOr[PassportNumber] = {
      validateParameter(passportNumber, ImpossiblePassportNumber)
    }

    def validatePerson(person: PersonDto): AllErrorsOr[Person] = (
      validateOwner(person.name),
      validatePersonAge(person.age),
      validatePassportNumber(person.passportNumber),
      validateBirthDate(person.date)
    ).mapN(Person)

    def validateCard(paymentCard: PaymentCardDto): AllErrorsOr[PaymentCard] = (
      validateCardNumber(paymentCard.cardNumber),
      validateCardDate(paymentCard.date),
      validateCardCvv(paymentCard.cvv)
    ).mapN(PaymentCard)

    def validate(personDto: PersonDto, paymentCardDto: PaymentCardDto): AllErrorsOr[Account] = (
      validatePerson(PersonDto(personDto.name, personDto.age, personDto.passportNumber, personDto.date)),
      validateCard(PaymentCardDto(paymentCardDto.cardNumber, paymentCardDto.date, paymentCardDto.cvv))
    ).mapN(Account)

  }

  def main(args: Array[String]): Unit = {

    import AccountValidator._

    println(
      validate(
        PersonDto("Din Smith", 20, "MP142293", "2001-05-25"),
        PaymentCardDto("3948576857492837", "2024-01-16", "810")
      )
    )

    println(
      validate(PersonDto("Din", 16, "MP142293", "1900-02-08"), PaymentCardDto("19485768592837", "2020-03-14", "14"))
    )

  }

}
