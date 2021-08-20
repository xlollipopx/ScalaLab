package com

import com.logic.{Cell, SpreadsheetProcessor, WordCell}
import com.data.LocalSpreadsheetParser
import com.view.FileSpreadsheetWriter


object Application {

  type ErrorMessage = String
  final case class Spreadsheet(spreadsheet: List[List[Cell]])

  def validatePath(path:  String): Either[ErrorMessage, String] =
    if(new java.io.File(path).exists) Right(path)
    else Left("Wrong path")



  def main(args: Array[String]): Unit = {
    val readPath = args(0)
    val writePath = args(1)

    for {
      validReadPath  <- validatePath(readPath)
      validWritePath <- validatePath(writePath)

      spreadsheetParser  = new LocalSpreadsheetParser(validReadPath)
      parsedSpreadsheet <- spreadsheetParser.parse()

      spreadsheetProcessor = new SpreadsheetProcessor
      processedSpreadsheet = spreadsheetProcessor.process(parsedSpreadsheet)
      spreadsheetWriter = new FileSpreadsheetWriter().write(validWritePath, processedSpreadsheet)
    } yield (print(parsedSpreadsheet))

  }

}
