package aoc

import scala.io.Source
import System.nanoTime

abstract class Day(day: Int) extends App {

  private def readInput(location: String): String = Source.fromFile(location).getLines().mkString("\n")

  protected def A(input: String): Any

  protected def B(input: String): Any

  protected def test()

  override def main(args: Array[String]): Unit = {
    require(day >= 1 && day <= 25)
    val fileLocation = s"src/aoc/days/input/day$day.txt"
    val input = readInput(fileLocation)
    test()
    var (result, time) = profile(A(input))
    println(s"A returned $result, took $time ms")
    var (resultB, timeB) = profile(B(input))
    println(s"B returned $resultB, took $timeB ms")
  }

  def profile[R](code: => R, t: Long = nanoTime) = (code, (nanoTime - t)/1000000)
}
