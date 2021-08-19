package com.logic

import com.Application.Spreadsheet


sealed trait Cell {
  def getValue: List[(String,String)]
}
abstract class NotExpressionCell(cell: String) extends Cell{
  override def getValue: List[(String, String)] = List((cell, ""))
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
      case cell if cell.matches("\\d+") => NumberCell(cell)
      case cell if cell.contains('\'') => WordCell(cell)
      case cell if cell.contains('#') => ErrorCell(cell)
      case cell if cell == "" => EmptyCell(cell)
      case _ => Expression(cell)
    }
  }
}

sealed trait Calculator[T] {
  def calculate(): T
}

final class MatrixProcessor{

  def calculate(matrix: Spreadsheet):Spreadsheet = {
    val res = matrix.spreadsheet.map(x => x.map(cell => cell match {
  case cell if cell.isInstanceOf[EmptyCell] => cell
  case cell if cell.isInstanceOf[EmptyCell] => cell

}))
    Spreadsheet(res)
  }

  def calculateCell(i: Int, j: Int, matrix:  Spreadsheet): Cell= {

    def calculate(i: Int, j: Int, matrix: Spreadsheet ): Cell= {
      val cell = matrix.spreadsheet(i)(j)

      cell match {
        case cell if cell.isInstanceOf[EmptyCell] =>  ErrorCell("#not all numbers")
        case cell if cell.isInstanceOf[WordCell] =>  ErrorCell("#not all numbers")
        case cell if cell.isInstanceOf[NumberCell] =>  cell
        case _ => Cell(cell.getValue.foldLeft(0)((acc, pair) =>
          if(isAllDigits(pair._2))  updateExpression(acc, pair._1, pair._2)
          else updateExpression(acc, pair._1,
            calculate(getCellIndex(pair._2)._1, getCellIndex(pair._2)._2, matrix).getValue.head._1)).toString)

      }

    }
    calculate(i,j,matrix)
  }

  private def updateExpression(current: Int, operation: String, x: String ): Int ={
    operation match{
      case "+"  => (current + x.toInt)
      case "-"  => (current - x.toInt)
      case "*"  => (current * x.toInt)
      case "/"  => (current / x.toInt)
//      case "/" if x == "0" => Cell("#/ by zero")
//      case operation if x.contains("#") =>Cell(current.getValue.head._1)
    }
  }

  private def getCellIndex(x: String): (Int, Int) = {
   val res= x.span( _== x.head)
    (res._2.toInt - 1, res._1.toCharArray.head - 'A')
  }

  private def isAllDigits(x: String): Boolean = x forall Character.isDigit

}
