import days._
import myfilereader.MyFileReader

object driver extends App {
  val lines = MyFileReader.read("src/days/input/day1.txt")
  Day1.test()
  println(Day1.run(lines))
}
