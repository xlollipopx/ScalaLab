package http

import http.Game.{StartInput, StartInputDto}
import http.StartInputValidator.InputValidationError.{
  InvalidAttemptsValue,
  InvalidBounds,
  InvalidMaxValue,
  InvalidMinValue
}

import scala.util.{Success, Try}

object StartInputValidator {

  sealed trait InputValidationError

  object InputValidationError {
    final case object InvalidBounds extends InputValidationError

    final case object InvalidMinValue extends InputValidationError

    final case object InvalidMaxValue extends InputValidationError

    final case object InvalidAttemptsValue extends InputValidationError
  }

  def validate(startInputDto: StartInputDto): Either[InputValidationError, StartInput] = {
    for {
      min      <- validateNumber(startInputDto.min, "[-+]?\\d+", InvalidMinValue)
      max      <- validateNumber(startInputDto.max, "[-+]?\\d+", InvalidMaxValue)
      _        <- Either.cond(min <= max, (min, max), InvalidBounds)
      attempts <- validateNumber(startInputDto.attempt, "[+]?\\d+", InvalidAttemptsValue)

    } yield StartInput(min, max, attempts)
  }

  def validateNumber(num: String, regex: String, error: InputValidationError): Either[InputValidationError, Int] = for {
    res <- Either.cond(num.matches(regex), num.toInt, error)
  } yield res

}
