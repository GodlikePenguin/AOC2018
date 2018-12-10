package aoc.days

import aoc.Day

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day10 extends Day(10) {

  def printStars(points: mutable.HashMap[(Int, Int), String]) = {
    for (y <- points.minBy(_._1._2)._1._2 to points.maxBy(_._1._2)._1._2) {
      for (x <- points.minBy(_._1._1)._1._1 to points.maxBy(_._1._1)._1._1) {
        print(points.getOrElse((x, y), " "))
      }
      println
    }
    println
  }

  override protected def A(input: String): Any = {
    var points = new ListBuffer[Point]()
    input.split("\n").foreach(line => {
      var nums = "(<.*> |<.*>$)".r.findAllIn(line).toList
      var coords = nums(0).replaceAll("<", "").replaceAll(">", "")
        .replaceAll("\\s", "").split(",").map(_.toInt)
      var velocity = nums(1).replaceAll("<", "").replaceAll(">", "")
        .replaceAll("\\s", "").split(",").map(_.toInt)
      points += new Point(coords(0), coords(1), velocity(0), velocity(1))
    })
    var starRepresentation = new mutable.HashMap[(Int, Int), String]()
    for (i <- 0 to 20000) {
      starRepresentation.clear()
      for (p <- points) {
        starRepresentation.put(p.getXY(), "#")
        p.update()
      }
      if ((starRepresentation.maxBy(_._1._2)._1._2 - starRepresentation.minBy(_._1._2)._1._2) < 10) {
        if ((starRepresentation.maxBy(_._1._1)._1._1 - starRepresentation.minBy(_._1._1)._1._1) < 100) {
          printStars(starRepresentation)
          println(s"took $i seconds")
        }
      }
    }
    println("=============================================================================================")
  }

  override protected def B(input: String): Any = {
    A(input)
  }

  override protected def test(): Unit = {
    A("position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>")
  }
}

class Point(var x: Int = 0, var y: Int = 0, var xv: Int = 0, var yv: Int = 0) {
  def update(): Unit = {
    this.x += this.xv
    this.y += yv
  }

  def getXY() : (Int, Int) = {
    (x, y)
  }
}
