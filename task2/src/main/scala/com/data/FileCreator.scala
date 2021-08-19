package com.data

import com.Application.Spreadsheet
import com.logic.Cell
import org.scalactic.ErrorMessage

import scala.io.Source
import scala.util.{Failure, Success, Try}

trait Parser[T]{
  def parse(): Either[ErrorMessage,T]
}

trait SpreadsheetParser extends Parser[Spreadsheet]{
}

class LocalSpreadsheetParser(path: String) extends SpreadsheetParser {
  override def parse(): Either[ErrorMessage, Spreadsheet] =
    Try(Source.fromFile(path).getLines()) match {
      case Success(lines) => Right(Spreadsheet(lines.drop(1).map(line=> line.split("\\s{4}")).toList.map(x => x.toList.map(y => Cell(y)))))
      case Failure(e) => Left("Could not read")
    }
}
