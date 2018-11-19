package myfilereader

import scala.io.Source

object MyFileReader {
  def read(fileName: String) : String  = {
    val bufferedSource = Source.fromFile(fileName)
    val lines = bufferedSource.getLines().mkString("\n")
    bufferedSource.close()
    lines
  }
}
