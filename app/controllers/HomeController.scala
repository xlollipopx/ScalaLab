package controllers

import javax.inject._
import play.api.mvc._
import services.Model.{Band, Car, Guitar}
import services.SparkService
import org.apache.spark.sql.{Encoder, Encoders}
import SparkService.spark.implicits._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {


  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def cars() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.cars(SparkService.readFromDb[Car]("cars")))
  }

  def bands() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.bands(SparkService.readFromDb[Band]("bands")))
  }

  def guitars() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.guitars(SparkService.readFromDb[Guitar]("guitars")))
  }
}
