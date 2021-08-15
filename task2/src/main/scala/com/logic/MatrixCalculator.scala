package com.logic

import java.util.StringTokenizer
import scala.Array.ofDim
import scala.collection.mutable.ArrayBuffer


sealed trait Calculator[T] {
  def calculate(): T
}

final case class MatrixCalculator (matrix: Array[Array[String]]) extends Calculator[Array[Array[String]]]{

  def calculate():Array[Array[String]] = {
    val result = ofDim[String](matrix.length, matrix(0).length)
    for(i <- matrix.indices){
      for(j <- matrix(0).indices){

        result(i)(j) = if(matrix(i)(j).charAt(0) == '\''){
          matrix(i)(j).substring(1)
        } else if(matrix(i)(j) == ""){
         matrix(i)(j)
        } else {
           calculateCell(i,j, matrix)
        }
      }
    }
    result
  }

  def calculateCell(i: Int, j: Int, matrix: Array[Array[String]] ): String= {
    /*This array helps to find cycles in matrix.
     *For example A1 has a reference to B1 and B1 has a reference to A1.
     * Array visited has three states:
     * 0 - haven't visited yet
     * 1 - cell is visited and stays in recursion stack
     * 2 - cell is visited and not in recursion stack */
    val visited = ofDim[Int](matrix.length, matrix(0).length)

    def calculate(i: Int, j: Int, matrix: Array[Array[String]] ): String= {
      val strCell = matrix(i)(j)

      if(visited(i)(j) == 1){
        return "#cycle"
      }
      if(strCell(0) == '\'' || strCell == ""){
        return "#not all numbers"
      }
      if(isAllDigits(strCell)) {
        return strCell
      }
      visited(i)(j) = 1
      val list = ArrayBuffer[String]()
      val stringTokenizer = new StringTokenizer(strCell, "+-*/=", true)
      while (stringTokenizer.hasMoreElements) {
        list += stringTokenizer.nextElement.toString
      }
      list(0) = "+"
      var result = 0
      var k = 0
      /* Calculating all operands in the cell recursively
      * and calculating the result */
      while(k < list.length - 1) {
        var operand = ""
        if(isAllDigits(list(k + 1))){
          operand = list(k + 1)
          if(operand == "0" && list(k) == "/"){
            return "#/ by zero"
          }
        } else {
          val a = list(k + 1).substring(1).toInt - 1
          val b = list(k + 1).charAt(0) - 'A'
          operand = calculate(a,b,matrix)
          visited(a)(b) = 2
          if(operand(0) == '#'){
            return operand
          }
          if(operand == "0" && list(k) == "/"){
            return "#/ by zero"
          }
        }
        val tmpNum = operand.toInt
        list(k) match{
          case "+" => result += tmpNum
          case "-" => result -= tmpNum
          case "*" => result *= tmpNum
          case "/" => result /= tmpNum
        }
        k += 2
      }
      result.toString
    }
    calculate(i,j,matrix)
  }

  private def isAllDigits(x: String): Boolean = x forall Character.isDigit

}
