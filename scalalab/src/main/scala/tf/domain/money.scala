package tf.domain

import io.circe.generic.JsonCodec

import java.util.Currency

object money {

  final case class Money(amount: BigDecimal, currency: Currency)

}
