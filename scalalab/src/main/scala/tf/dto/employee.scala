package tf.dto

import io.circe.generic.JsonCodec

object employee {

  @JsonCodec
  final case class EmployeeDto(
    birthday:    String,
    firstName:   String,
    lastName:    String,
    salary:      String,
    currency:    String,
    position:    String,
    is_archived: Boolean
  )

}
