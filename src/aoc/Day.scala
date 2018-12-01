package aoc

import scala.io.Source

abstract class Day(day: Int) extends App {

  private def readInput(location: String): String = Source.fromFile(location).getLines().mkString("\n")

  protected def test()

  protected def A(input: String): AnyVal

  protected def B(input: String): AnyVal

  override def main(args: Array[String]): Unit = {
    require(day >= 1 && day <= 25)
    val fileLocation = s"src/aoc/days/input/day$day.txt"
    val input = readInput(fileLocation)
    test()
    println(A(input))
    println(B(input))
  }
}
