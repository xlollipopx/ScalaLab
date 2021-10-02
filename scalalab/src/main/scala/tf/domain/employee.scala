package tf.domain
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import tf.domain.money.Money

import java.time.LocalDate
import java.util.UUID

object employee {
  type Name     = String Refined MatchesRegex[W.`"^(([A-Za-z]+ ?){1,3})$"`.T]
  type Position = String Refined MatchesRegex[W.`"[A-Z][a-z]+"`.T]

  final case class Employee(
    id:        Long,
    birthday:  LocalDate,
    firstName: String,
    lastName:  String,
    salary:    Money,
    position:  String
  )

}
