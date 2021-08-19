package com

import com.logic.{Cell, MatrixProcessor, WordCell}
import com.data.LocalSpreadsheetParser
import com.view.FileMatrixPrinter


object Application {

  type ErrorMessage = String
  final case class Spreadsheet(spreadsheet: List[List[Cell]])

  def validatePath(path:  String): Either[ErrorMessage, String] =
    if(new java.io.File(path).exists) Right(path)
    else Left("Wrong path")



  def main(args: Array[String]): Unit = {
    val readPath = args(0)
    val writePath = args(1)

    val sph =new  LocalSpreadsheetParser(readPath)
    val mpr = new MatrixProcessor()
   print(mpr.calculateCell(1,2, sph.parse().toOption.get))

    for {
      validReadPath  <- validatePath(readPath)
      validWritePath <- validatePath(writePath)

      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)

      parsedSpreadsheet <- spreadsheetParser.parse()

        spreadsheetProcessor = new MatrixProcessor().calculateCell(1,3, parsedSpreadsheet)


      //      spreadsheetWriter = new LocalSpreadsheetWriter
      //      _                <- spreadsheetWriter.write(validWritePath, processedSpreadsheet)
    } print(spreadsheetProcessor)


  }

}
