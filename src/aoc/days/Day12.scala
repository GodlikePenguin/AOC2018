package aoc.days

import aoc.Day

import scala.collection.mutable
import scala.reflect.internal.util.HashSet
import scala.util.control.Breaks

object Day12 extends Day(12) {
  override protected def A(input: String): Any = {
    var padding = "." * 25
    var states = padding + input.split("\n")(0).split(": ")(1) + padding
    var hashRules =  new mutable.HashSet[String]()
    var dotRules = new mutable.HashSet[String]()
    "(.){5} => #".r.findAllIn(input).foreach(l => hashRules.add(l.split(" ")(0)))
    "(.){5} => .".r.findAllIn(input).foreach(l => dotRules.add(l.split(" ")(0)))
    for (i <- 0 until 20) {
      var nextState = ""
      for (j <- 0 until states.length) {
        var substring = ""
        if (j==0) substring = ".." + states.substring(j, j+3)
        else if (j==1) substring = "." + states.substring(j-1, j+3)
        else if (j==states.length-1) substring = states.substring(j-2) + ".."
        else if (j==states.length-2) substring = states.substring(j-2) + "."
        else substring = states.substring(j-2, j+3)

        if (hashRules.contains(substring)) {
          nextState += '#'
        } else if (dotRules.contains(substring)) {
          nextState += '.'
        } else {
          nextState += states(j)
        }
      }
      states = nextState
    }
    states.zipWithIndex.foldLeft(0)((accum, pair) => if (pair._1=='#') accum + pair._2-padding.length else accum)
  }

  override protected def B(input: String): Any = {
    var leftpadding = "." * 10
    var rightPadding = "." * 300
    var states = leftpadding + input.split("\n")(0).split(": ")(1) + rightPadding
    var hashRules =  new mutable.HashSet[String]()
    var dotRules = new mutable.HashSet[String]()
    "(.){5} => #".r.findAllIn(input).foreach(l => hashRules.add(l.split(" ")(0)))
    "(.){5} => .".r.findAllIn(input).foreach(l => dotRules.add(l.split(" ")(0)))
    val range = Iterator.from(0)
    Breaks.breakable {
      for (i <- range) {
        if (i == 200) Breaks.break
        var nextState = ""
        for (j <- 0 until states.length) {
          var substring = ""
          if (j == 0) substring = ".." + states.substring(j, j + 3)
          else if (j == 1) substring = "." + states.substring(j - 1, j + 3)
          else if (j == states.length - 1) substring = states.substring(j - 2) + ".."
          else if (j == states.length - 2) substring = states.substring(j - 2) + "."
          else substring = states.substring(j - 2, j + 3)

          if (hashRules.contains(substring)) {
            nextState += '#'
          } else if (dotRules.contains(substring)) {
            nextState += '.'
          } else {
            nextState += states(j)
          }
        }
        states = nextState
      }
    }
    states.zipWithIndex.foldLeft(0L)((accum, pair) => if (pair._1=='#') accum + pair._2-leftpadding.length + 50000000000L - 200 else accum)
  }

  def stream(i: Long = 1): Stream[Long] = i #:: stream(i + 1)

  override protected def test(): Unit = {
//    assert(A("initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #")==325)
  }
}
