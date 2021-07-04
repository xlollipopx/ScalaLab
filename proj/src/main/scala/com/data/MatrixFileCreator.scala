package com.data

import java.io.File
import scala.Array.ofDim
import scala.io.Source

class MatrixFileCreator {

  def create(filePath: String):Array[Array[String]] = {
    val source = Source.fromFile(filePath)
    val lines = source.getLines()
    lines.drop(1)
    val matrix = lines.map(line=> line.split("\\s{4}"))
    matrix.toArray
  }
}
