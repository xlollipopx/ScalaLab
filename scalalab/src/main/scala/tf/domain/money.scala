package tf.domain

import java.util.Currency
import io.circe._

import scala.util.Try

object money {

  final case class Money(amount: BigDecimal, currency: Currency)
}
