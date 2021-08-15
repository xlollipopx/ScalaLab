package com.data

import java.io.File
import scala.Array.ofDim
import scala.io.Source



sealed trait Creator[T]{
  def create(): T
}

sealed trait MatrixCreator extends Creator[Array[Array[String]]]{
}

final case class MatrixFileCreator(filePath: String) extends MatrixCreator {

  def create():Array[Array[String]] = {
    val source = Source.fromFile(filePath)
    val lines = source.getLines()
    lines.drop(1)
    val matrix = lines.map(line=> line.split("\\s{4}"))
    matrix.toArray
  }
}
