package aoc.days

import aoc.Day

import scala.collection.mutable

object Day13 extends Day(13) {
  override protected def A(input: String): Any = {
    var (maze, carts) = prepareMazeAndCarts(input)
    var crashed = false
    var crashLocation = (-1, -1)
    while (!crashed) {
      for (cart <- carts.toList.sortWith(classSorter)) {
        cart.update(maze)
        for (other <- carts) {
          if (other.coordinate == cart.coordinate && other.cartId != cart.cartId) {
            crashLocation = cart.coordinate
            crashed = true
          }
        }
      }
    }
    crashLocation
  }

  override protected def B(input: String): Any = {
    var (maze, carts) = prepareMazeAndCarts(input)
    while (carts.size > 1) {
      for (cart <- carts.toList.sortWith(classSorter)) {
        cart.update(maze)
        for (other <- carts) {
          if (other.coordinate == cart.coordinate && other.cartId != cart.cartId) {
            carts.remove(other)
            carts.remove(cart)
          }
        }
      }
    }
    carts.toList(0).coordinate
  }

  override protected def test(): Unit = {
    assert(A("/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   ")==(7, 3))
  }

  def prepareMazeAndCarts(input : String): (mutable.HashMap[(Int, Int), String], mutable.HashSet[Cart]) = {
    val maze = new mutable.HashMap[(Int, Int), String]()
    var carts = new mutable.HashSet[Cart]()
    var currentCartId = 0
    input.split("\n").zipWithIndex.foreach(line => {
      line._1.split("").zipWithIndex.foreach(char => {
        if (char._1 == ">") {
          maze.put((char._2, line._2), "-")
          carts.add(new Cart(coordinate = (char._2, line._2), facing = 1, cartId = currentCartId))
          currentCartId += 1
        } else if (char._1 == "v") {
          maze.put((char._2, line._2), "|")
          carts.add(new Cart(coordinate = (char._2, line._2), facing = 2, cartId = currentCartId))
          currentCartId += 1
        } else if (char._1 == "^") {
          maze.put((char._2, line._2), "|")
          carts.add(new Cart(coordinate = (char._2, line._2), facing = 0, cartId = currentCartId))
          currentCartId += 1
        } else if (char._1 == "<") {
          maze.put((char._2, line._2), "-")
          carts.add(new Cart(coordinate = (char._2, line._2), facing = 3, cartId = currentCartId))
          currentCartId += 1
        } else {
          maze.put((char._2, line._2), char._1)
        }
      })
    })
    (maze, carts)
  }

  def classSorter(c1 : Cart, c2 : Cart) : Boolean = {
    c1.coordinate._1 < c2.coordinate._1 || (c1.coordinate._1 == c2.coordinate._1 && c1.coordinate._2 < c2.coordinate._2)
  }
}

class Cart(var coordinate : (Int, Int) = (0, 0), var nextTurn : Int = 0, var facing : Int = 0, var cartId : Int) {
  def update(maze : mutable.HashMap[(Int, Int), String]): Unit = {
    if (maze(coordinate) == "+") {
      if (nextTurn == 0) {
        facing = (facing + 3) % 4
      } else if (nextTurn == 1) {
        //go ahead, noop
      } else if (nextTurn == 2) {
        facing = (facing + 1) % 4
      }
      nextTurn = (nextTurn + 1) % 3
    } else if (maze(coordinate) == "\\") {
      if (facing == 0 || facing == 2) {
        facing = (facing + 3) % 4
      } else if (facing == 1 || facing == 3) {
        facing = (facing + 1) % 4
      }
    } else if (maze(coordinate) == "/") {
      if (facing == 0 || facing == 2) {
        facing = (facing + 1) % 4
      } else if (facing == 1 || facing == 3) {
        facing = (facing + 3) % 4
      }
    }

    if (facing == 0) {
      coordinate = (coordinate._1, coordinate._2 - 1)
    } else if (facing == 1) {
      coordinate = (coordinate._1 + 1, coordinate._2)
    } else if (facing == 2) {
      coordinate = (coordinate._1, coordinate._2 + 1)
    } else if (facing == 3) {
      coordinate = (coordinate._1 - 1, coordinate._2)
    }
  }
}
