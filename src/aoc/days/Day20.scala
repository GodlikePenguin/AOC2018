package aoc.days

import aoc.Day

import scala.collection.mutable

object Day20 extends Day(20) {
  override protected def A(input: String): Any = {
    val regex = input.substring(1, input.length-1)
    val nodes = new mutable.HashSet[Node]()
    var current = new Node(0, 0)
    nodes.add(current)
    val edges = new mutable.HashSet[(Node, Node)]()
    var savedState = new Node(0,0)
    for (i <- 0 until regex.length) {
      if (regex(i) != '(' && regex(i) != '|' && regex(i) != ')') {
        var next = new Node(0,0)
        regex(i) match {
          case 'N' => next = new Node(current.x, current.y-1)
          case 'E' => next = new Node(current.x+1, current.y)
          case 'S' => next = new Node(current.x+1, current.y)
          case 'W' => next = new Node(current.x+1, current.y)
        }
        edges.add((current, next))
        current = next
        nodes.add(current)
      } else if (regex(i) == '(') {
        savedState = current
      } else if (regex(i) == '|') {
        current = savedState
      } else if (regex(i) == ')') {
        current = savedState
      }
    }
  }

  override protected def B(input: String): Any = {}

  override protected def test(): Unit = {
    assert(A("^WNE$")==3)
  }
}

class Node(var x: Int, var y: Int) {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Node]

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Node => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  }

  override def hashCode(): Int = {
    this.x.hashCode() + this.y.hashCode()
  }
}
