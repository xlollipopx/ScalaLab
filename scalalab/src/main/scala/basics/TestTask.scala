package basics

import java.io.{BufferedWriter, FileWriter}
import scala.io.Source
import scala.util.{Failure, Success, Try}

object TestTask {

  type ErrorMessage = String

  final case class Spreadsheet(spreadsheet: List[List[Cell]])

  def validatePath(path: String): Either[ErrorMessage, String] =
    if (new java.io.File(path).exists) Right(path)
    else Left("Wrong path")

  sealed trait Cell {
    def getValue: List[(String, String)]
  }

  abstract class NotExpressionCell(cell: String) extends Cell {
    override def getValue: List[(String, String)] = List((cell, "+"))

    override def toString: String = cell
  }

  case class Expression(cell: String) extends Cell {
    override def getValue: List[(String, String)] = {
      val list = "+" :: cell.split("[\\s=]+").toList.tail
      list
        .foldLeft(List.empty[(String, String)] -> (None: Option[String])) {
          case ((result, Some(key)), value) => (result :+ (key -> value)) -> None
          case ((result, None), key)        => result          -> Some(key)
        }
        ._1
    }
  }

  final case class WordCell(cell: String) extends NotExpressionCell(cell) with Cell

  final case class ErrorCell(cell: String) extends NotExpressionCell(cell) with Cell

  final case class NumberCell(cell: String) extends NotExpressionCell(cell) with Cell

  final case class EmptyCell(cell: String) extends NotExpressionCell(cell) with Cell

  object Cell {
    def apply(cell: String): Cell = {
      cell match {
        case cell if cell.matches("[+-]?\\d+") => NumberCell(cell)
        case cell if cell.contains('\'')       => WordCell(cell)
        case cell if cell.contains('#')        => ErrorCell(cell)
        case cell if cell == ""                => EmptyCell(cell)
        case _                                 => Expression(cell)
      }
    }
  }

  trait Processor {
    def process(spreadsheet: Spreadsheet): Spreadsheet
  }

  final class SpreadsheetProcessor extends Processor {

    def process(spreadsheet: Spreadsheet): Spreadsheet = {
      val res = spreadsheet.spreadsheet.map(x =>
        x.map {
          case cell: NumberCell => cell
          case cell if cell.isInstanceOf[WordCell]  => cell
          case cell if cell.isInstanceOf[ErrorCell] => cell
          case cell if cell.isInstanceOf[EmptyCell] => cell

          case cell => processCell(cell, spreadsheet)
        }
      )
      Spreadsheet(res)
    }

    def processCell(cell: Cell, matrix: Spreadsheet): Cell = {
      def process(cell: Cell, matrix: Spreadsheet): Cell = {

        cell match {
          case cell: EmptyCell => ErrorCell("#not all numbers")
          case cell if cell.isInstanceOf[NumberCell] => cell
          case _ =>
            Cell(
              cell.getValue
                .foldLeft(0) { case (acc, (operation, x)) =>
                  if (isAllDigits(x)) updateExpression(acc, operation, x)
                  else
                    updateExpression(
                      acc,
                      operation,
                      process(matrix.spreadsheet(getCellIndex(x)._1)(getCellIndex(x)._2), matrix).getValue.head._1
                    )
                }
                .toString
            )
        }
      }
      process(cell, matrix)
    }

    private def updateExpression(current: Int, operation: String, x: String): Int = {
      operation match {
        case "+" => (current + x.toInt)
        case "-" => (current - x.toInt)
        case "*" => (current * x.toInt)
        case "/" => (current / x.toInt)
      }
    }

    private def getCellIndex(x: String): (Int, Int) = {
      val res = x.span(_ == x.head)
      (res._2.toInt - 1, res._1.toCharArray.head - 'A')
    }

    private def isAllDigits(x: String): Boolean = x forall Character.isDigit
  }

  trait Parser[T] {
    def parse(): Either[ErrorMessage, T]
  }

  trait SpreadsheetParser extends Parser[Spreadsheet] {}

  class LocalSpreadsheetParser(path: String) extends SpreadsheetParser {
    override def parse(): Either[ErrorMessage, Spreadsheet] =
      Try(Source.fromFile(path).getLines()) match {
        case Success(lines) =>
          Right(
            Spreadsheet(lines.drop(1).map(line => line.split("\\s{4}")).toList.map(x => x.toList.map(y => Cell(y))))
          )
        case Failure(e) => Left("Could not read")
      }
  }

  trait SpreadsheetWriter {
    def write(writePath: String, processedSpreadsheet: Spreadsheet): Either[ErrorMessage, Spreadsheet]
  }

  class FileSpreadsheetWriter extends SpreadsheetWriter {

    def write(writePath: String, processedSpreadsheet: Spreadsheet): Either[ErrorMessage, Spreadsheet] = {
      val writer = new BufferedWriter(new FileWriter(writePath))
      val matrix = processedSpreadsheet.spreadsheet.map(x => x.map(y => y.toString))
      val result = buildText(matrix)
      writer.write(result)
      Right(processedSpreadsheet)
    }
  }

  def buildText(matrix: List[List[String]]): String = {
    matrix
      .map(line => buildLine(line))
      .reduceLeft((x, y) => x + "\n" + y)
  }

  def buildLine(line: List[String]): String = {
    line.reduceLeft((x, y) => x + "\t" + y)
  }

  def main(args: Array[String]): Unit = {
    val readPath  = args(0)
    val writePath = args(1)

    for {
      validReadPath  <- validatePath(readPath)
      validWritePath <- validatePath(writePath)

      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)
      parsedSpreadsheet <- spreadsheetParser.parse()

      spreadsheetProcessor = new SpreadsheetProcessor
      processedSpreadsheet = spreadsheetProcessor.process(parsedSpreadsheet)
      spreadsheetWriter    = new FileSpreadsheetWriter
      _                   <- spreadsheetWriter.write(validWritePath, processedSpreadsheet)
    } yield (print(parsedSpreadsheet))

  }

}
