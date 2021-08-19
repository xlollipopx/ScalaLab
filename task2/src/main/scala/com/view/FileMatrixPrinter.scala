package com.view
import java.io.{ DataOutputStream, File, FileOutputStream}


sealed trait Printer[T]{
  def print(): Unit
}

sealed trait MatrixPrinter extends Printer[List[List[String]]]{
}

final case class FileMatrixPrinter(filePath: String, matrix: List[List[String]]) extends MatrixPrinter {

  def print(): Unit ={
    val dataOutputStream =
      new DataOutputStream(new FileOutputStream(new File("src/main/resources/output.txt")))
    for (row <- matrix) {
      for(el <- row){
        dataOutputStream.writeBytes(el)
        dataOutputStream.writeBytes("    ")
      }
      dataOutputStream.writeBytes("\n")
    }
    dataOutputStream.close()
  }

}
