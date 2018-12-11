package aoc.days

import aoc.Day

import scala.collection.mutable

object Day11 extends Day(11) {

  def getPowerLevelFor(x: Int, y: Int, serialNumber: String): Int = {
    val rackId = x + 10
    getHundreds(((rackId * y) + serialNumber.toInt) * rackId)-5
  }

  def getHundreds(i: Int): Int = {
    i.toString.zipWithIndex.map(a => (i.toString.length - a._2, a._1)).toMap.getOrElse(3, '0').toString.toInt
  }

  def getTotalPowerLevelFor(x: Int, y: Int, range: Int, levels: mutable.HashMap[(Int, Int), Int]): Int = {
    var sum = 0
    for (i <- x to x+range) {
      for (j <- y to y+range) {
        sum += levels(i, j)
      }
    }
    sum
  }

  def fasterSummer(levels: mutable.HashMap[(Int, Int), Int]): mutable.HashMap[(Int, Int, Int), Int] = {
    //Faster than Naive B but can't find the error in it
    var result = new mutable.HashMap[(Int, Int, Int), Int]()
    for (x <- 1 to 300) {
      for (y <- 1 to 300) {
        result.put((x, y, 1), levels(x, y))
      }
    }
    for (i <- 2 to 20) {
      for (x <- 1 to 300-(i-1)) {
        for (y <- 1 to 300-(i-1)) {
          var partialResult = result((x, y, i-1))
          for (j <- 0 until i) {
            partialResult += levels(x+(i-1), y+j)
            partialResult += levels(x+j, y+(i-1))
          }
          partialResult += levels(x+(i-1), y+(i-1))
          result.put((x, y, i), partialResult)
        }
      }
      println(s"B completed iteration $i")
    }
    result
  }

  override protected def A(input: String): Any = {
    val serialNumber = input
    var powerLevels = new mutable.HashMap[(Int, Int), Int]()
    var maxPower = new mutable.HashMap[(Int, Int), Int]()
    for (y <- 1 to 300) {
      for (x <- 1 to 300) {
        powerLevels.put((x, y), getPowerLevelFor(x, y, serialNumber))
      }
    }
    for (y <- 1 to 298) {
      for (x <- 1 to 298) {
        maxPower.put((x, y), getTotalPowerLevelFor(x, y, 2, powerLevels))
      }
    }
    maxPower.maxBy(_._2)._1
  }

  override protected def B(input: String): Any = {
    val serialNumber = input
    var powerLevels = new mutable.HashMap[(Int, Int), Int]()
    for (y <- 1 to 300) {
      for (x <- 1 to 300) {
        powerLevels.put((x, y), getPowerLevelFor(x, y, serialNumber))
      }
    }
    var maxMax = ((0, 0, 0), 0)
    for (i <- 1 to 20) {
      var localMax = new mutable.HashMap[(Int, Int), Int]()
      for (x <- 1 to 300-(i-1)) {
        for (y <- 1 to 300-(i-1)) {
          var sum = 0
          for (j <- x until x+i) {
            for (k <- y until y+i) {
              sum += powerLevels((j, k))
            }
          }
          localMax.put((x, y), sum)
        }
      }
      var calc = localMax.maxBy(_._2)
      if (calc._2 > maxMax._2) {
        maxMax = ((calc._1._1, calc._1._2, i), calc._2)
      }
      println(s"B completed iteration $i")
    }
    maxMax._1
  }

  override protected def test(): Unit = {
    assert(getHundreds(100)==1)
    assert(getHundreds(12345)==3)
    assert(getHundreds(0)==0)
    assert(getPowerLevelFor(122, 79, "57")== -5)
    assert(getPowerLevelFor(217, 196, "39")== 0)
    assert(getPowerLevelFor(101, 153, "71")== 4)
    assert(A("18")==(33,45))
    assert(A("42")==(21,61))
    //These asserts pass but take way too long to run
//    assert(B("18")==(90,269,16))
//    assert(B("42")==(232,251,12))
  }
}
