package aoc.days

import aoc.Day

import scala.collection.mutable

object Day9 extends Day(9){
  override protected def A(input: String): Any = {
    val nums = "[0-9]+".r.findAllIn(input).toList
    val players = nums(0).toInt
    val lastMarble = nums(1).toInt
    var scores = new mutable.HashMap[Int, Int]()
    var currentPlayer = 1
    var currentMarble = new Marble(value = 0)
    currentMarble.next = currentMarble
    currentMarble.prev = currentMarble
    for(i <- 1 to lastMarble) {
      var a = new Marble(value = i)
      if (i%23==0) {
        currentMarble = currentMarble.prev.prev.prev.prev.prev.prev.prev.prev
        var old = currentMarble.removeNext()
        currentMarble = currentMarble.next
        scores.put(currentPlayer, scores.getOrElse(currentPlayer, 0)+i+old.value)
      } else {
        currentMarble = currentMarble.next
        currentMarble.insertNext(newMarble = a)
        currentMarble = a
      }
      currentPlayer = (currentPlayer + 1) % players
    }
    val toReturn = scores.maxBy(_._2)._2
    toReturn
  }

  override protected def B(input: String): Any = {
    val nums = "[0-9]+".r.findAllIn(input).toList
    val players = nums(0).toInt
    val lastMarble = nums(1).toInt
    var scores = new mutable.HashMap[Int, Long]()
    var currentPlayer = 1
    var currentMarble = new Marble(value = 0)
    currentMarble.next = currentMarble
    currentMarble.prev = currentMarble
    for(i <- 1 to lastMarble*100) {
      var a = new Marble(value = i)
      if (i%23==0) {
        currentMarble = currentMarble.prev.prev.prev.prev.prev.prev.prev.prev
        var old = currentMarble.removeNext()
        currentMarble = currentMarble.next
        scores.put(currentPlayer, scores.getOrElse(currentPlayer, 0L)+i+old.value)
      } else {
        currentMarble = currentMarble.next
        currentMarble.insertNext(newMarble = a)
        currentMarble = a
      }
      currentPlayer = (currentPlayer + 1) % players
    }
    val toReturn = scores.maxBy(_._2)._2
    toReturn
  }

  override protected def test(): Unit = {
    assert(A("9 players; last marble is worth 25 points")==32)
    assert(A("10 players; last marble is worth 1618 points")==8317)
    assert(A("13 players; last marble is worth 7999 points")==146373)
    assert(A("17 players; last marble is worth 1104 points")==2764)
    assert(A("21 players; last marble is worth 6111 points")==54718)
    assert(A("30 players; last marble is worth 5807 points")==37305)
  }
}

class Marble(var value: Int = 0, var next: Marble = null, var prev: Marble = null) {
  def insertNext(newMarble: Marble): Unit  = {
    var prevNext = next
    next = newMarble
    newMarble.prev = this
    newMarble.next = prevNext
    prevNext.prev = newMarble
  }

  def removeNext(): Marble = {
    var prevNext = next
    next = prevNext.next
    next.prev = this
    prevNext
  }
}
