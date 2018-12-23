package aoc.days

import aoc.Day

import scala.collection.mutable

object Day23 extends Day(23) {
  override protected def A(input: String): Any = {
    val nanobots = new mutable.HashSet[((Long, Long, Long), Long)]()
    input.split("\n").foreach(line => {
      val posS = line.split(", ")(0)
      val rS = line.split(", ")(1)
      val r = rS.substring(2).toLong
      val pos = posS.substring(5, posS.length-1).split(",").map(_.toLong)
      nanobots.add(((pos(0), pos(1), pos(2)), r))
    })
    val maxR = nanobots.maxBy(_._2)
    nanobots.count(bot => {
      Math.abs(bot._1._1 - maxR._1._1) + Math.abs(bot._1._2 - maxR._1._2) + Math.abs(bot._1._3 - maxR._1._3) <= maxR._2
    })
  }

  override protected def B(input: String): Any = {
    val nanobots = new mutable.HashSet[((Long, Long, Long), Long)]()
    input.split("\n").foreach(line => {
      val posS = line.split(", ")(0)
      val rS = line.split(", ")(1)
      val r = rS.substring(2).toLong
      val pos = posS.substring(5, posS.length-1).split(",").map(_.toLong)
      nanobots.add(((pos(0), pos(1), pos(2)), r))
    })
    var closest = ((0L,0L,0L), -10)

    for (z <- nanobots.minBy(_._1)._1._3 to nanobots.maxBy(_._1)._1._3) {
      for (y <- nanobots.minBy(_._1)._1._2 to nanobots.maxBy(_._1)._1._2) {
        for (x <- 0L to nanobots.maxBy(_._1)._1._1) {
          var localCount = 0
          nanobots.foreach(bot => {
             if (Math.abs(bot._1._1 - x) + Math.abs(bot._1._2 - y) + Math.abs(bot._1._3 - z) <= bot._2) localCount += 1
          })
          if (localCount > closest._2) {
            closest = ((x,y,z), localCount)
            println(s"new closest $closest")
          } else if (localCount == closest._2) {
            if (Math.abs(x) + Math.abs(y) + Math.abs(z) < Math.abs(closest._1._1) + Math.abs(closest._1._2) + Math.abs(closest._1._3)) {
              closest = ((x,y,z), localCount)
            }
          }
        }
      }
    }
    Math.abs(closest._1._1) + Math.abs(closest._1._2) + Math.abs(closest._1._3)
  }

  override protected def test(): Unit = {
    assert(A("pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1")==7)
    assert(B("pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5")==36)
  }
}
