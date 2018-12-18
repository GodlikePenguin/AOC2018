package aoc.days

import aoc.Day

import scala.collection.mutable

object Day18 extends Day(18) {
  protected def work(input: String, length: Int): Any = {
    var trees = new mutable.HashMap[(Int, Int), String]()
    input.split("\n").zipWithIndex.foreach(line => {
      line._1.zipWithIndex.foreach(char => trees.put((char._2, line._2), char._1.toString))
    })

    var seenMaps = new mutable.HashMap[Int, mutable.HashMap[(Int, Int), String]]()

    var counter = 0
    while (counter < Math.min(length, 564)) {
      var newTrees = new mutable.HashMap[(Int, Int), String]()
      trees.foreach(tree => {
        if (tree._2 == ".") {
          if (getSurrounding(tree._1, trees, "|") >= 3) {
            newTrees.put(tree._1, "|")
          } else {
            newTrees.put(tree._1, ".")
          }
        } else if (tree._2 == "|") {
          if (getSurrounding(tree._1, trees, "#") >= 3) {
            newTrees.put(tree._1, "#")
          } else {
            newTrees.put(tree._1, "|")
          }
        } else if (tree._2 == "#") {
          if (getSurrounding(tree._1, trees, "|") >= 1 && getSurrounding(tree._1, trees, "#") >= 1) {
            newTrees.put(tree._1, "#")
          } else {
            newTrees.put(tree._1, ".")
          }
        }
      })
      trees = newTrees
      counter += 1
    }

    if (length > 564) {
      counter = 0
      while (counter < (length-564)%28) {
        var newTrees = new mutable.HashMap[(Int, Int), String]()
        trees.foreach(tree => {
          if (tree._2 == ".") {
            if (getSurrounding(tree._1, trees, "|") >= 3) {
              newTrees.put(tree._1, "|")
            } else {
              newTrees.put(tree._1, ".")
            }
          } else if (tree._2 == "|") {
            if (getSurrounding(tree._1, trees, "#") >= 3) {
              newTrees.put(tree._1, "#")
            } else {
              newTrees.put(tree._1, "|")
            }
          } else if (tree._2 == "#") {
            if (getSurrounding(tree._1, trees, "|") >= 1 && getSurrounding(tree._1, trees, "#") >= 1) {
              newTrees.put(tree._1, "#")
            } else {
              newTrees.put(tree._1, ".")
            }
          }
        })
        trees = newTrees
        counter += 1
      }
    }
//    printTrees(trees)
    trees.count(_._2 == "|") * trees.count(_._2 == "#")
  }

  override protected def A(input: String): Any = {
    work(input, 10)
  }

  def getSurrounding(point: (Int, Int), trees: mutable.HashMap[(Int, Int), String], token: String): Int = {
    var tot = 0
    for (y <- point._2-1 to point._2+1) {
      for (x <- point._1-1 to point._1+1) {
        if (trees.getOrElse((x, y), "") == token && (x, y) != point) tot += 1
      }
    }
    tot
  }

  def printTrees(trees: mutable.HashMap[(Int, Int), String]) = {
    for (y <- 0 to trees.maxBy(_._1._2)._1._2) {
      for (x <- 0 to trees.maxBy(_._1._1)._1._1) {
        print(trees(x, y))
      }
      println
    }
  }

  override protected def B(input: String): Any = {
    work(input, 1000000000)
  }

  override protected def test(): Unit = {
    assert(A(".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.")==1147)
  }
}
