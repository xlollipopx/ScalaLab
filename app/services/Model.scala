package services

object Model {

  case class Car(
                  Name: String,
                  Miles_per_Gallon: Option[Double],
                  Cylinders: Long,
                  Displacement: Double,
                  Horsepower: Option[Long],
                  Weight_in_lbs: Long,
                  Acceleration: Double,
                  Year: String,
                  Origin: String
                )

  case class Guitar(id: Long, make: String, model: String, guitarType: String)
  case class Band(id: Long, name: String, hometown: String, year: Long)
}
