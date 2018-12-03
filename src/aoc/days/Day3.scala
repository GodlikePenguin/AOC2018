package aoc.days

import aoc.Day

import scala.collection.mutable

object Day3 extends Day(3) {
  override protected def A(input: String): Int = {
    var cloth = new mutable.HashMap[(Int, Int), Int]()
    for (line <- input.split("\n")) {
      val pair = line.split(" ")(2).split(",")
      val startX = Integer.parseInt(pair(0))
      val startY = Integer.parseInt(pair(1).replace(":",""))
      val range = line.split(" ")(3)
      val xrange = Integer.parseInt(range.split("x")(0))
      val yrange = Integer.parseInt(range.split("x")(1))
      for (i <- 0 until xrange) {
        for (j <- 0 until yrange) {
          val p = (startX+i, startY+j)
          val pre = cloth.get(p)
          if (pre.isEmpty) {
            cloth.put(p, 1)
          } else {
            cloth.put(p, pre.get+1)
          }
        }
      }
    }
    cloth.count(e => e._2 > 1)
  }

  override protected def B(input: String): Int = {
    var validSet = new mutable.HashSet[Int]()
    var cloth = new mutable.HashMap[(Int, Int), mutable.Set[Int]]()
    for (line <- input.split("\n")) {
      val id = Integer.parseInt(line.split(" ")(0).replace("#", ""))
      val pair = line.split(" ")(2).split(",")
      val startX = Integer.parseInt(pair(0))
      val startY = Integer.parseInt(pair(1).replace(":",""))
      val range = line.split(" ")(3)
      val xrange = Integer.parseInt(range.split("x")(0))
      val yrange = Integer.parseInt(range.split("x")(1))
      var notModifiedPrevious = true
      for (i <- 0 until xrange) {
        for (j <- 0 until yrange) {
          val p = (startX+i, startY+j)
          val pre = cloth.get(p)
          if (pre.isEmpty) {
            cloth.put(p, mutable.Set(id))
          } else {
            notModifiedPrevious = false
            val prevVals = pre.get
            validSet = validSet -- prevVals
            cloth.put(p, prevVals + id)
          }
        }
      }
      if (notModifiedPrevious) validSet.add(id)
    }
    //Should only be one element so cheat and sum the one
    validSet.sum
  }

  override protected def test(): Unit = {
    assert(A("#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")==4)
    assert(B("#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")==3)
  }

  def printmap(pointToInt: mutable.HashMap[(Int, Int), Int]): Unit = {
    for (x <- 0 to 8) {
      for (y <- 0 to 8) {
        val c = pointToInt.get((x, y))
        if (c.isEmpty) print(".")
        else print(c.get)
      }
      println
    }
  }
}
