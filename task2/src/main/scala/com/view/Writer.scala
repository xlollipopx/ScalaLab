package com.view
import com.Application.{ErrorMessage, Spreadsheet}
import java.io.{BufferedWriter,  FileWriter}


trait SpreadsheetWriter {
  def write(writePath: String, processedSpreadsheet: Spreadsheet):   Spreadsheet
}

final class FileSpreadsheetWriter extends SpreadsheetWriter {

  def write(writePath: String, processedSpreadsheet: Spreadsheet):   Spreadsheet = {
    val writer = new BufferedWriter(new FileWriter(writePath))
    processedSpreadsheet.spreadsheet.foreach { x =>
      x.foreach(y => writer.write(y.toString + "   "))
      writer.write("\n")
    }
    processedSpreadsheet
  }
}
