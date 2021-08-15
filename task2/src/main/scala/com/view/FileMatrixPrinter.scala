package com.view

import java.io.BufferedWriter
import java.io.{File, FileWriter}


class FileMatrixPrinter  {

  def print(filePath: String, matrix: Array[Array[String]]): Unit ={
    val file = new File(filePath)
    val bw = new BufferedWriter(new FileWriter(file))
    for (row <- matrix) {
      for(el <- row){
        bw.write(el)
        bw.write("    ")
      }
      bw.write('\n')
    }
    bw.close()
  }

}
