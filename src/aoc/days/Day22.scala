package aoc.days

import aoc.Day

import scala.collection.mutable

object Day22 extends Day(22) {


  override protected def A(input: String): Any = {
    doWork(input).foldLeft(0)((a, b) => a + b._2 % 3)
  }

  def doWork(input: String): mutable.HashMap[(Int, Int), Int] = {
    val depth = input.split("\n")(0).split(" ")(1).toInt
    val targetCoords = input.split("\n")(1).split(" ")(1).split(",")
    val targetX = targetCoords(0).toInt
    val targetY = targetCoords(1).toInt
    val threatLevels = new mutable.HashMap[(Int, Int), Int]()
    for (y <- 0 to targetY) {
      threatLevels.put((0, y), calculateThreatLevelFor(0, y, threatLevels, depth))
    }
    for (x <- 0 to targetX) {
      threatLevels.put((x, 0), calculateThreatLevelFor(x, 0, threatLevels, depth))
    }
    for (y <- 1 to targetY) {
      for (x <- 1 to targetX) {
        threatLevels.put((x, y), calculateThreatLevelFor(x, y, threatLevels, depth))
      }
    }
    threatLevels.put((targetX, targetY), calculateThreatLevelFor(0, 0, threatLevels, depth))
    threatLevels
  }

  def calculateThreatLevelFor(x: Int, y: Int, threatLevels: mutable.HashMap[(Int, Int), Int], depth: Int): Int = {
    if (x == 0 && y == 0) {
      return depth % 20183
    } else if (x == 0) {
      return ((y * 48271) + depth) % 20183
    } else if (y == 0) {
      return ((x * 16807) + depth) % 20183
    } else {
      return ((threatLevels((x - 1, y)) * threatLevels((x, y-1))) + depth) % 20183
    }
  }

  override protected def B(input: String): Any = {
    doWork(input)
  }

  override protected def test(): Unit = {
    assert(A("depth: 510\ntarget: 10,10")==114)
  }
}
