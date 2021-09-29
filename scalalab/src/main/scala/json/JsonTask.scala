package json

import io.circe._
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import io.circe.jawn.decode
import io.circe.syntax.EncoderOps
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import json.Models.{Todo, User}

import java.net.{URI, URL}
import java.time.LocalDate
import scala.io.Source
import scala.util.{Failure, Success, Try}

object JsonTask {

  type ErrorMessage = String

  def getUrlContent(url: URL): Either[ErrorMessage, String] = {
    Try(Source.fromURL(url).getLines()) match {
      case Success(lines) => Right(lines.foldLeft("")((acc, el) => acc + el + "\n"))
      case Failure(e)     => Left("Could not read from resource")
    }
  }

  def parseJson[T](content: String)(implicit decoder: Decoder[T]): Either[Error, List[T]] = {
    parser.decode[List[T]](content)
  }

  def main(args: Array[String]): Unit = {
    import Models._
    val postsUrl    = new URL(" https://jsonplaceholder.typicode.com/posts")
    val commentsUrl = new URL("https://jsonplaceholder.typicode.com/comments")
    val albumsUrl   = new URL("https://jsonplaceholder.typicode.com/albums")
    val todosUrl    = new URL("https://jsonplaceholder.typicode.com/todos")
    val usersUrl    = new URL("https://jsonplaceholder.typicode.com/users")

    for {
      todos          <- getUrlContent(todosUrl)
      parsedTodos    <- parseJson[Todo](todos)
      users          <- getUrlContent(usersUrl)
      parsedUsers    <- parseJson[User](users)
      albums         <- getUrlContent(albumsUrl)
      parsedAlbums   <- parseJson[Album](albums)
      comments       <- getUrlContent(commentsUrl)
      parsedComments <- parseJson[Comment](comments)
      posts          <- getUrlContent(postsUrl)
      parsedPosts    <- parseJson[Post](posts)

    } {
      println(parsedUsers)
      println(parsedAlbums)
      println(parsedComments)
      println(parsedPosts)
      println(parsedTodos)
    }

  }
}
