package controllers

import javax.inject._
import play.api.mvc._
import services.Model.{Band, Car, Guitar}
import services.SparkService
import org.apache.spark.sql.{Encoder, Encoders}
import SparkService.spark.implicits._
import org.json4s.jackson.Json
import play.api.http.Writeable.wByteArray
import play.api.libs.circe.Circe

import org.apache.commons.io.FilenameUtils
import io.circe.syntax._
import org.apache.commons.lang3.exception.ExceptionUtils

import io.circe.generic.auto._
import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
import play.api.libs.json.{JsError, JsPath, Writes}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc)  with Circe {



  def getTable(name: String) = Action { implicit request: Request[AnyContent] =>
    Ok(SparkService.readDFFromDb(name).toJSON.collect().asJson)
  }

  def upload = Action(parse.multipartFormData) { request =>
    request.body
      .file("file")
      .map { file =>
        val filename    = Paths.get(file.filename).getFileName
        file.ref.copyTo(Paths.get(s"conf/data/$filename"), replace = true)
        val df = SparkService.readDfFromJson(filename.toString)
        SparkService.writeToDb(df, FilenameUtils.removeExtension(filename.toString))
        Ok("File uploaded")
      }
      .getOrElse {
        BadRequest
      }
  }



}
