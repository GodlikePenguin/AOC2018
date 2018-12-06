package aoc.days

import aoc.Day

import scala.collection.mutable

object Day6 extends Day(6) {

  def printMap(start: Int, end: Int, grid: mutable.HashMap[(Int, Int), Int]): Unit = {
    for (y <- start to end) {
      for (x <- start to end) {
        print(grid.getOrElse((x, y), ".") + "\t")
      }
      println
    }
    println
  }

  def closestPointsFor(start: Int, end :Int, grid: mutable.HashMap[(Int, Int), Int]): mutable.HashMap[(Int, Int), Int] = {
    val closestPointMap = new mutable.HashMap[(Int, Int), Int]()
    for (x <- start to end) {
      for (y <- start to end) {
        var closestManH = Integer.MAX_VALUE
        var id = -1
        grid.foreach(entry => {
          val ManH = Math.abs(entry._1._1-x) + Math.abs(entry._1._2-y)
          if (ManH < closestManH) {
            closestManH = ManH
            id = entry._2
          } else if (ManH == closestManH) { //two points are closest, put a dot)
            id = -1
          }
        })
        closestPointMap.put((x, y), id)
      }
    }
    closestPointMap
  }

  override protected def A(input: String): Any = {
    var grid = new mutable.HashMap[(Int, Int), Int]()
    var currentId = 1
    input.split("\n").foreach(line => {
      var x = line.split(", ")(0).toInt
      var y = line.split(", ")(1).toInt
      grid.put((x, y), currentId)
      currentId += 1
    })

    //start with 400x400 map (this encompasses all points) then increase the size
    // pick all the ids which don't increase their volume.
    val closestPointMap400 = closestPointsFor(0, 400, grid)
    val closestPointMap500 = closestPointsFor(-100, 500, grid)
    var maximumFiniteArea = 0
    val areain400 = closestPointMap400.groupBy(_._2)
    val areain500 = closestPointMap500.groupBy(_._2)

    areain400.foreach(entry => {
      if (areain500.get(entry._1).nonEmpty && entry._2.size == areain500(entry._1).size) {
        if (entry._2.size > maximumFiniteArea) {
          maximumFiniteArea = entry._2.size
        }
      }
    })

    maximumFiniteArea
  }

  override protected def B(input: String): Any = {
    var grid = new mutable.HashMap[(Int, Int), Int]()
    var currentId = 1
    input.split("\n").foreach(line => {
      var x = line.split(", ")(0).toInt
      var y = line.split(", ")(1).toInt
      grid.put((x, y), currentId)
      currentId += 1
    })

    val closestPointMap400 = closestPointsFor(0, 400, grid)

    var squares = 0
    closestPointMap400.foreach(a => {
      var tmpSum = 0
      grid.foreach(b => {
        tmpSum += Math.abs(a._1._1-b._1._1) + Math.abs(a._1._2-b._1._2)
      })
      if (tmpSum < 10000) {
        squares += 1
      }
    })
    squares
  }

  override protected def test(): Unit = {
    assert(A("1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9")==17)
  }
}
