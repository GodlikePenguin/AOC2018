package aoc.days

import aoc.Day

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

object Day8 extends Day(8) {
  override protected def A(input: String): Any = {
    var mutableInput = input.split(" ").toList
    var total = 0
    var kind = 0 // 0 -> children number, 1 -> meta number, 2 -> meta value
    var childrenStack = ListBuffer[Int]()
    var metaStack = ListBuffer[Int]()
    for (char <- mutableInput) {
      Breaks.breakable {
        if (kind == 0) {
          childrenStack += char.toInt
          kind = 1
        }

        else if (kind == 1) {
          metaStack += char.toInt
          if (childrenStack.last == 0) {
            kind = 2
          } else {
            kind = 0
          }
        }

        else if (kind == 2) {
          var add = metaStack.last
          metaStack.remove(metaStack.length - 1)
          total += char.toInt
          if (add == 1) { // done with adding
            var childrenLeft = childrenStack.last
            while (childrenLeft == 0) {
              childrenStack.remove(childrenStack.length - 1)
              if (childrenStack.isEmpty) {
                kind = 0
                Breaks.break
              }
              childrenLeft = childrenStack.last
            }
            childrenLeft -= 1 // done with this child
            childrenStack.remove(childrenStack.length-1)
            childrenStack += childrenLeft
            if (childrenLeft == 0) {
              kind = 2
            } else {
              kind = 0
            }
          } else {
            metaStack += add - 1
          }
        }
      }
    }
    total
  }

  override protected def B(input: String): Any = {
    var mutableInput = input.split(" ").zipWithIndex
    parseNode(mutableInput)._1.getValue()
  }

  def parseNode(chars: Array[(String, Int)]): (Node, Array[(String, Int)]) = {
    var mutableChars = chars
    var numChildren = mutableChars(0)._1.toInt
    var numMeta = mutableChars(1)._1.toInt
    mutableChars = mutableChars.takeRight(mutableChars.length-2)
    var node = new Node
    for (i <- 0 until numChildren) {
      var (child, restOfChars) = parseNode(mutableChars)
      node.children += child
      mutableChars = restOfChars
    }
    for (i <- 0 until numMeta) {
      node.meta += mutableChars(i)._1.toInt
    }
    mutableChars = mutableChars.takeRight(mutableChars.length-numMeta)
    (node, mutableChars)
  }



  override protected def test(): Unit = {
    assert(A("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")==138)
    println(B("1 1 0 1 10 1"))

    assert(B("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")==66)
  }
}

class Node(var children : ListBuffer[Node] = ListBuffer[Node](), var meta : ListBuffer[Int] = ListBuffer[Int]()) {
  def getValue(): Int = {
    if (children.length > 0) meta.foldLeft(0)((accum, i) => accum + (if (i < children.length+1) children(i-1).getValue() else 0))
    else meta.sum
  }
}
