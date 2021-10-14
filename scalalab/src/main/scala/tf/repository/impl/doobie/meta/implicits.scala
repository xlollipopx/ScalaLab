package tf.repository.impl.doobie.meta

import doobie.{Meta, Read, Write}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import tf.domain.employee._
import tf.domain.money.Money

import java.time.LocalDate
import java.util.{Currency, UUID}
import java.sql.Date

object implicits {

  implicit val moneyMeta: Meta[Money] =
    Meta[String]
      .timap(s => toMoney(s))(g => g.amount.toString() + " " + g.currency.toString)

  implicit val dateMeta: Meta[LocalDate] =
    Meta[String]
      .timap(s => LocalDate.parse(s))(g => g.toString)

  implicit val nameMeta: Meta[Name] =
    Meta[String]
      .timap(s => Refined.unsafeApply(s).asInstanceOf[Name])(g => g.value)

  implicit val positionMeta: Meta[Position] =
    Meta[String]
      .timap(s => Refined.unsafeApply(s).asInstanceOf[Position])(g => g.value)

  def toMoney(s: String): Money = {
    val list = s.split("\\s").toList
    Money(BigDecimal(list.head), Currency.getInstance(list.last))
  }

  //  implicit val uuidMeta: Meta[UUID] =
  //    Meta[String].imap[UUID](UUID.fromString)(_.toString)

}
