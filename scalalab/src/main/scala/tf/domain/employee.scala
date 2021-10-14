package tf.domain

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import io.circe.generic.JsonCodec
import money.Money

import java.time.LocalDate
import java.time.{Instant, LocalDate}
import java.util.UUID

object employee {

  type Name     = String Refined MatchesRegex[W.`"^(([A-Za-z]+ ?){1,3})$"`.T]
  type Position = String Refined MatchesRegex[W.`"^[a-zA-Z]*$"`.T]

  final case class Employee(
    id:          UUID,
    birthday:    LocalDate,
    firstName:   Name,
    lastName:    Name,
    salary:      Money,
    position:    Position,
    is_archived: Boolean
  )

}
