package errors

import errors.Errors.AccountValidationError.{
  AgeIsOutOfBounds,
  DateIsNotValid,
  ImpossibleCvv,
  ImpossiblePassportNumber,
  InvalidCardNumber,
  NameIsInvalid
}
import errors.Errors.AccountValidator._
import cats.data._
import cats.syntax.all._
import errors.Errors.{Account, AccountDto, AccountValidationError, PaymentCardDto, Person, PersonDto}
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.string.MatchesRegex
import org.scalatest.freespec.AnyFreeSpec
import cats.data.Chain
import cats.data.Validated.Invalid

class ErrorHandlingSpec extends AnyFreeSpec {

  def checkValid[S, T](input: S, error: AccountValidationError)(f: S => AllErrorsOr[T]): Unit = {
    val actual = f(input)
    assert(actual.isValid)
    actual.fold(
      e => fail(s"Doesn't expect an error: $e"),
      v => assert(v.toString == input.toString)
    )
  }

  def checkInvalid[S, T](input: S, error: AccountValidationError)(f: S => AllErrorsOr[T]): Unit = {
    val actual = f(input)
    assert(actual.isInvalid)
    actual.fold(
      e => assert(e === Chain(error)),
      v => fail(s"Doesn't expect a Valid value: $v")
    )
  }

  "cvv" - {
    "valid cvv: 444" in {
      val inputSecurityCode = "123"
      checkValid(inputSecurityCode, ImpossibleCvv)(validateCardCvv)
    }

    "invalid cvv: 3212d" in {
      val inputSecurityCode = "3212d"
      checkInvalid(inputSecurityCode, ImpossibleCvv)(validateCardCvv)
    }
  }

  "owner" - {
    "valid name" in {
      val inputName = "Ivan Levi"
      checkValid(inputName, NameIsInvalid)(validateOwner)
    }

    "invalid name: with numbers" in {
      val inputName = "Ivan23"
      checkInvalid(inputName, NameIsInvalid)(validateOwner)
    }

    "invalid name: empty" in {
      val inputName = ""
      checkInvalid(inputName, NameIsInvalid)(validateOwner)
    }
  }

  "card expiration date" - {
    "valid date" in {
      val inputDate = "2025-11-14"
      checkValid(inputDate, DateIsNotValid)(validateCardDate)
    }

    "invalid date: before now" in {
      val inputDate = "2001-11-14"
      checkInvalid(inputDate, DateIsNotValid)(validateCardDate)
    }
    "invalid date: wrong format" in {
      val inputDate = "2026-13-14"
      checkInvalid(inputDate, DateIsNotValid)(validateCardDate)
    }
    "invalid date: empty" in {
      val inputDate = ""
      checkInvalid(inputDate, DateIsNotValid)(validateCardDate)
    }
  }

  "birthday" - {
    "valid date" in {
      val inputDate = "2000-11-14"
      checkValid(inputDate, DateIsNotValid)(validateBirthDate)
    }

    "invalid date: after now" in {
      val inputDate = "2025-11-14"
      checkInvalid(inputDate, DateIsNotValid)(validateBirthDate)
    }
    "invalid date: wrong format" in {
      val inputDate = "2000-13-14"
      checkInvalid(inputDate, DateIsNotValid)(validateBirthDate)
    }
    "invalid date: empty" in {
      val inputDate = ""
      checkInvalid(inputDate, DateIsNotValid)(validateBirthDate)
    }
  }

  "age" - {
    "valid age" in {
      val inputAge = 18
      checkValid(inputAge, AgeIsOutOfBounds)(validatePersonAge)
    }
    "Invalid age: less then 18" in {
      val inputAge = 17
      checkInvalid(inputAge, AgeIsOutOfBounds)(validatePersonAge)
    }

    "Invalid age: negative" in {
      val inputAge = -3
      checkInvalid(inputAge, AgeIsOutOfBounds)(validatePersonAge)
    }

    "passport number" - {
      "valid passport number" in {
        val inputPn = "MP142293"
        checkValid(inputPn, ImpossiblePassportNumber)(validatePassportNumber)
      }
      "Invalid passport number: only numbers" in {
        val inputPn = "23142293"
        checkInvalid(inputPn, ImpossiblePassportNumber)(validatePassportNumber)
      }
      "Invalid passport number: contains spaces" in {
        val inputPn = "MP2314 2293"
        checkInvalid(inputPn, ImpossiblePassportNumber)(validatePassportNumber)
      }
    }

    "person" - {
      "valid person" in {
        val rawName   = "Igor"
        val rawBirth  = "2001-04-19"
        val rawAge    = 20
        val rawPn     = "MP124323456"
        val rawPerson = PersonDto(rawName, rawAge, rawPn, rawBirth)
        val expected  = rawPerson.valid
        val actual = for {
          person <- validatePerson(rawPerson)
          name    = person.name.value
          age     = person.age.value
          birth   = person.birthDate
          pn      = person.passportNumber.value
        } yield PersonDto(name, age, pn, birth)
        assert(expected == actual)
      }
      "invalid person: all fields wrong" in {
        val rawName  = "Igor1"
        val rawBirth = "2027-04-19"
        val rawAge   = 0
        val rawPn    = "123456"
        val expected = Invalid(Chain(NameIsInvalid, AgeIsOutOfBounds, ImpossiblePassportNumber, DateIsNotValid))
        val actual   = validatePerson(PersonDto(rawName, rawAge, rawPn, rawBirth))
        assert(actual == expected)
      }
    }

    "payment card" - {
      "valid card" in {
        val rawCardNum = "3948576857492837"
        val rawDate    = "2024-01-05"
        val rawCvv     = "242"
        val rawCard    = PaymentCardDto(rawCardNum, rawDate, rawCvv)
        val expected   = rawCard.valid
        val actual = for {
          card   <- validateCard(rawCard)
          cardNum = card.cardNumber.value
          date    = card.expirationDate
          cvv     = card.cvv.value
        } yield PaymentCardDto(cardNum, date, cvv)
        assert(actual == expected)
      }

      "invalid card: all fields are wrong" in {
        val rawCardNum = "76857492831"
        val rawDate    = "2020-01-05"
        val rawCvv     = "22"
        val expected   = Invalid(Chain(InvalidCardNumber, DateIsNotValid, ImpossibleCvv))
        val actual     = validateCard(PaymentCardDto(rawCardNum, rawDate, rawCvv))
        assert(actual == expected)
      }
    }

    "account" - {
      "valid account" in {
        val rawName    = "Igor"
        val rawBirth   = "2001-04-19"
        val rawAge     = 20
        val rawPn      = "MP124323456"
        val rawPerson  = PersonDto(rawName, rawAge, rawPn, rawBirth)
        val rawCardNum = "3948576857492837"
        val rawDate    = "2024-01-05"
        val rawCvv     = "242"
        val rawCard    = PaymentCardDto(rawCardNum, rawDate, rawCvv)
        val rawAccount = AccountDto(rawPerson, rawCard)
        val expected   = rawAccount.valid
        val actual = for {
          account <- validate(rawPerson, rawCard)
          name     = account.person.name.value
          age      = account.person.age.value
          pn       = account.person.passportNumber.value
          birth    = account.person.birthDate
          cardNum  = account.card.cardNumber.value
          date     = account.card.expirationDate
          cvv      = account.card.cvv.value
        } yield AccountDto(PersonDto(name, age, pn, birth), PaymentCardDto(cardNum, date, cvv))
        assert(actual == expected)
      }

      "invalid account" in {
        val rawName    = "Denis3"
        val rawBirth   = "2020-14-19"
        val rawAge     = 13
        val rawPn      = "124323456"
        val rawPerson  = PersonDto(rawName, rawAge, rawPn, rawBirth)
        val rawCardNum = "39485768574928V"
        val rawDate    = "2020-01-05"
        val rawCvv     = "42"
        val rawCard    = PaymentCardDto(rawCardNum, rawDate, rawCvv)

        val expected = Invalid(
          Chain(
            NameIsInvalid,
            AgeIsOutOfBounds,
            ImpossiblePassportNumber,
            DateIsNotValid,
            InvalidCardNumber,
            DateIsNotValid,
            ImpossibleCvv
          )
        )
        val actual = validate(rawPerson, rawCard)
        assert(actual == expected)
      }
    }
  }
}
