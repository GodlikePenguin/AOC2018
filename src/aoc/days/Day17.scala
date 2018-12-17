package aoc.days

import aoc.Day

import scala.collection.mutable
import scala.util.control.Breaks

object Day17 extends Day(17) {
  override protected def A(input: String): Any = {
    var landscape = mutable.HashMap[(Int, Int), String]()
    landscape.put((500, 0), "+")
    input.split("\n").foreach(line => {
      var x = 0 to 0
      var y = 0 to 0
      line.split(", ").foreach(coordinate => {
        if (coordinate.contains("x")) {
          if (coordinate.contains("..")) {
            var nums = coordinate.substring(2).split("\\.\\.")
            var lhs = nums(0).toInt
            var rhs = nums(1).toInt
            x = lhs to rhs
          } else {
            x = coordinate.substring(2).toInt to coordinate.substring(2).toInt
          }
        } else {
          if (coordinate.contains("..")) {
            var nums = coordinate.substring(2).split("\\.\\.")
            var lhs = nums(0).toInt
            var rhs = nums(1).toInt
            y = lhs to rhs
          } else {
            y = coordinate.substring(2).toInt to coordinate.substring(2).toInt
          }
        }
      })
      for (i <- x) {
        for (j <- y) {
          landscape.put((i, j), "#")
        }
      }
    })

    val minY = landscape.filter(a => a._2 != "+").minBy(_._1._2)._1._2
    val maxY = landscape.maxBy(_._1._2)._1._2
//    println(minY)
//    println(maxY)
//    printMap(landscape)
//    println
    var waterQueue = new mutable.Queue[(Int, Int)]()
    waterQueue = waterQueue :+ (500, 0)
    while (waterQueue.nonEmpty) {
      Breaks.breakable {
        var current = waterQueue.last
        if (current._2 > maxY) {
          landscape.put(current, "")
          while(waterQueue.contains(current)) {
            waterQueue.dequeueFirst(a => a == current) //remove the current element from the queue
            current = (current._1, current._2-1)
          }
          Breaks.break
        }

        if (landscape.getOrElse((current._1, current._2+1), "") == "") {
          landscape.put((current._1, current._2+1), "|") //flowing water
          waterQueue = waterQueue :+ ((current._1, current._2+1))
        } else {
          if (landscape.getOrElse((current._1, current._2+1), "") == "~") {
            var scanner = current
            //check right for already existing spill
            while (landscape.getOrElse((scanner._1+1, scanner._2+1), "") == "~") {
              scanner = (scanner._1+1, scanner._2)
            }
            if (landscape.getOrElse((scanner._1+1, scanner._2+1), "") == "|") {
              while(waterQueue.contains(current)) {
                waterQueue.dequeueFirst(a => a == current) //remove the current element from the queue
                current = (current._1, current._2-1)
              }
              Breaks.break
            }

            //check left
            scanner = current
            while (landscape.getOrElse((scanner._1-1, scanner._2+1), "") == "~") {
              scanner = (scanner._1-1, scanner._2)
            }
            if (landscape.getOrElse((scanner._1-1, scanner._2+1), "") == "|") {
              while(waterQueue.contains(current)) {
                waterQueue.dequeueFirst(a => a == current) //remove the current element from the queue
                current = (current._1, current._2-1)
              }
              Breaks.break
            }
          }

          //check if we land on a single obstacle (this happens once in my input and isn't handled correctly
          if (landscape.getOrElse((current._1, current._2+1), "") == "#" &&
          landscape.getOrElse((current._1-1, current._2+1), "") != "#" &&
          landscape.getOrElse((current._1+1, current._2+1), "") != "#") {
            landscape.put(current, "~")
            waterQueue.dequeueFirst(a => a == current)
            landscape.put((current._1+1, current._2), "|")
            waterQueue = waterQueue :+ (current._1+1, current._2)
            landscape.put((current._1-1, current._2), "|")
            waterQueue = waterQueue :+ (current._1-1, current._2)
          }

          landscape.put(current, "~") //resting water
          //go right
          var nextStanding = current
          while (landscape.getOrElse((nextStanding._1+1, nextStanding._2), "") == "" && landscape.getOrElse((nextStanding._1+1, nextStanding._2+1), "") != "") {
            nextStanding = (nextStanding._1+1, nextStanding._2)
            landscape.put(nextStanding, "~")
          }
          if (landscape.getOrElse((nextStanding._1+1, nextStanding._2), "") == "" && landscape.getOrElse((nextStanding._1+1, nextStanding._2+1), "") == "") {
            landscape.put((nextStanding._1+1, nextStanding._2), "|")
            waterQueue = waterQueue :+ (nextStanding._1+1, nextStanding._2)
          }
          //go left
          nextStanding = current
          while (landscape.getOrElse((nextStanding._1-1, nextStanding._2), "") == "" && landscape.getOrElse((nextStanding._1-1, nextStanding._2+1), "") != "") {
            nextStanding = (nextStanding._1-1, nextStanding._2)
            landscape.put(nextStanding, "~")
          }
          if (landscape.getOrElse((nextStanding._1-1, nextStanding._2), "") == "" && landscape.getOrElse((nextStanding._1-1, nextStanding._2+1), "") == "") {
            landscape.put((nextStanding._1-1, nextStanding._2), "|")
            waterQueue = waterQueue :+ (nextStanding._1-1, nextStanding._2)
          }

          waterQueue.dequeueFirst(a => a == current) //remove the current element from the queue
        }
      }
//      printMap(landscape)
//      println
    }
    printMap(landscape)
    landscape.count(a => a._1._2 <= maxY && a._1._2 >= minY && (a._2 == "|" || a._2 == "~"))
  }

  def printMap(landscape: mutable.HashMap[(Int, Int), String]): Unit = {
    for (y <- 0 to landscape.maxBy(_._1._2)._1._2) {
      for (x <- 0 to landscape.maxBy(_._1._1)._1._1) {
        print(landscape.getOrElse((x, y), "."))
      }
      println()
    }
  }

  override protected def B(input: String): Any = {}

  override protected def test(): Unit = {
    assert(A("x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504")==57)
  }
}
