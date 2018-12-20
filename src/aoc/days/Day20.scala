package aoc.days

import aoc.Day

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day20 extends Day(20) {
  override protected def A(input: String): Any = {
    val regex = input.substring(1, input.length-1)
    val nodes = new mutable.HashSet[(Int, Int)]()
    val edges = new mutable.HashSet[((Int, Int), (Int, Int))]()
    val paths = expand(regex)
    for (path <- paths) {
      explorePath(path, nodes, edges)
    }
    BFS(nodes, edges).maxBy(_._2)._2
  }

  def BFS(nodes: mutable.HashSet[(Int, Int)], edges: mutable.HashSet[((Int, Int), (Int, Int))]): mutable.HashMap[(Int, Int), Int] = {
    var distances = new mutable.HashMap[(Int, Int), Int]()
    var queue = new mutable.Queue[((Int, Int), Int)]()
    var explored = new mutable.HashSet[(Int, Int)]()
    queue.enqueue(((0,0), 0))
    while(queue.nonEmpty) {
      var current = queue.dequeue()
      distances.put(current._1, current._2)
      explored.add(current._1)
      for (node <- edges.filter(_._1 == current._1)) {
        if (!explored.contains(node._2)) {
          queue.enqueue((node._2, current._2 + 1))
        }
      }
    }
    distances
  }

  def expand(regexS: String): Array[String] = {
    var currentStrings = Array[String]("")
    var i = 0
    while (i < regexS.length) {
      if (regexS(i) == '(') {
        val (a, b) = dealWithBrackets(regexS, i+1, currentStrings)
        i = a
        currentStrings = b
      } else {
        for (j <- currentStrings.indices) {
          currentStrings(j) += regexS(i)
        }
        i += 1
      }
    }
    currentStrings
  }

  def dealWithBrackets(str: String, index: Int, currentStrings: Array[String]): (Int, Array[String]) = {
    var localStrings = currentStrings
    var localCounter = index
    var sides = ArrayBuffer[Array[String]](Array[String](""))
    var currentSide = 0
    while(str(localCounter) != ')') {
      if (str(localCounter) == '(') {
        val (a, b) = dealWithBrackets(str, localCounter+1, sides(currentSide))
        localCounter = a
        sides(currentSide) = b
      } else if (str(localCounter)=='|') {
        currentSide += 1
        sides += Array[String]("")
        localCounter += 1
      } else {
        for (i <- sides(currentSide).indices) {
          sides(currentSide)(i) += str(localCounter)
        }
        localCounter += 1
      }
    }
    var newStrings = ArrayBuffer[String]()
    for (string <- localStrings) {
      for (side <- sides) {
        for (s <- side) {
          newStrings += string + s
        }
      }
    }
    localStrings = newStrings.toArray

    (localCounter+1, localStrings)
  }

  def explorePath(path: String, nodes: mutable.HashSet[(Int, Int)], edges: mutable.HashSet[((Int, Int), (Int, Int))]): Unit = {
    var current = (0, 0)
    nodes.add(current)
    for (i <- 0 until path.length) {
      var next = (0,0)
      path(i) match {
        case 'N' => next = (current._1, current._2-1)
        case 'E' => next = (current._1+1, current._2)
        case 'S' => next = (current._1, current._2+1)
        case 'W' => next = (current._1-1, current._2)
      }
      edges.add((current, next))
      current = next
      nodes.add(current)
    }
  }

  override protected def B(input: String): Any = {
    val regex = input.substring(1, input.length-1)
    val nodes = new mutable.HashSet[(Int, Int)]()
    val edges = new mutable.HashSet[((Int, Int), (Int, Int))]()
    val paths = expand(regex)
    for (path <- paths) {
      explorePath(path, nodes, edges)
    }
    BFS(nodes, edges).count(_._2 > 1000)
  }

  override protected def test(): Unit = {
    assert(A("^WNE$")==3)
    assert(A("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")==23)
    assert(A("^ENWWW(NEEE|SSE(EE|N))$")==10)
    assert(A("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")==18)
    assert(A("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")==31)
  }
}
