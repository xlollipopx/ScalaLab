package async

import async.Async.ec

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import cats.syntax.traverse._

import scala.util.{Failure, Success}

object Async {
  implicit private val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def main(args: Array[String]): Unit = {
    val ans = for {
      pageBody   <- fetchPageBody(args.head)
      pageLinks  <- findLinkUrls(pageBody)
      serverNames = pageLinks.map(fetchServerName)
      listNames  <- serverNames.traverse(identity)
      list        = listNames.distinct.flatten.sorted
    } yield list

    ans.onComplete {
      case Success(value)     => print(value)
      case Failure(exception) => exception.printStackTrace()
    }

  }
  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }

}
