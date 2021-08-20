package com.logic

import com.Application.Spreadsheet

import scala.annotation.tailrec


sealed trait Cell {
  def getValue: List[(String,String)]
}
abstract class NotExpressionCell(cell: String) extends Cell{
  override def getValue: List[(String, String)] = List((cell, "+"))
  override def toString: String = cell
}
case class Expression(cell: String) extends Cell {
  override def getValue: List[(String,String)] = {
    val list = "+"::cell.split("[\\s=]+").toList.tail
    list.foldLeft(List.empty[(String, String)] -> (None: Option[String])) {
      case ((result, Some(key)), value) =>  (result :+ (key -> value)) -> None
      case ((result, None), key) =>         result -> Some(key)
    }._1
  }
}
case class WordCell(cell: String) extends NotExpressionCell(cell) with Cell
case class ErrorCell(cell: String) extends NotExpressionCell(cell) with Cell
case class NumberCell(cell: String) extends NotExpressionCell(cell) with Cell
case class EmptyCell(cell: String) extends NotExpressionCell(cell) with Cell

object Cell {
  def apply(cell: String):Cell  ={
    cell match {
      case cell if cell.matches("[+-]?\\d+") => NumberCell(cell)
      case cell if cell.contains('\'') => WordCell(cell)
      case cell if cell.contains('#') => ErrorCell(cell)
      case cell if cell == "" => EmptyCell(cell)
      case _ => Expression(cell)
    }
  }
}
trait Processor {
  def process(spreadsheet: Spreadsheet): Spreadsheet
}

final class SpreadsheetProcessor extends Processor {

  def process(spreadsheet: Spreadsheet):Spreadsheet = {
    val res = spreadsheet.spreadsheet.map(x => x.map {
      case cell if cell.isInstanceOf[EmptyCell] => cell
      case cell if cell.isInstanceOf[WordCell] => cell
      case cell if cell.isInstanceOf[NumberCell] => cell
      case cell => processCell(cell, spreadsheet)
    })
    Spreadsheet(res)
  }

  def processCell(cell: Cell, matrix:  Spreadsheet): Cell= {
def process(cell: Cell, matrix: Spreadsheet ): Cell= {

      cell match {
        case cell if cell.isInstanceOf[EmptyCell] =>  ErrorCell("#not all numbers")
        case cell if cell.isInstanceOf[WordCell] =>  ErrorCell("#not all numbers")
        case cell if cell.isInstanceOf[NumberCell] =>  cell
        case _ => Cell(cell.getValue.foldLeft(0)((acc, pair) =>
          if(isAllDigits(pair._2))  updateExpression(acc, pair._1, pair._2)
          else updateExpression(acc, pair._1,
            process(matrix.spreadsheet(getCellIndex(pair._2)._1)(getCellIndex(pair._2)._2),
              matrix).getValue.head._1)).toString)
      }

    }
    process(cell, matrix)
  }

  private def updateExpression(current: Int, operation: String, x: String ): Int ={
    operation match{
      case "+"  => (current + x.toInt)
      case "-"  => (current - x.toInt)
      case "*"  => (current * x.toInt)
      case "/"  => (current / x.toInt)
    }
  }

  private def getCellIndex(x: String): (Int, Int) = {
   val res= x.span( _== x.head)
    (res._2.toInt - 1, res._1.toCharArray.head - 'A')
  }

  private def isAllDigits(x: String): Boolean = x forall Character.isDigit

}
