package com

import com.logic.MatrixCalculator
import com.data.MatrixFileCreator
import com.view.FileMatrixPrinter

import java.io.File
import scala.Array.ofDim
import scala.io.Source
import java.util.StringTokenizer
import scala.:+
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object Application {

  def main(args: Array[String]): Unit = {
    val inputPath = sys.env("INPUT")
    val matrix = (MatrixFileCreator(inputPath)).create()
    val result =  (MatrixCalculator(matrix)).calculate()
    print(result.map(_.mkString("    ")).mkString("\n"))
    val outputPath = sys.env("OUTPUT")
    val filePrinter = new FileMatrixPrinter
    filePrinter.print(outputPath, result)
  }


}
