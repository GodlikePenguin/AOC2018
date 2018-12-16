package aoc.days

import aoc.Day

import scala.collection.mutable

object Day16 extends Day(16) {
  override protected def A(input: String): Any = {
    input.split("\n").grouped(4).foldLeft(0)((accum, group) => {
      if (!group(0).contains("[")) return accum
      var instruction = group(1).split(" ").map(_.toInt)
      var beforeString = group(0)
      var before = beforeString.substring(9).substring(0, 10).split(", ").map(_.toInt)
      var afterString = group(2)
      var after = afterString.substring(9).substring(0, 10).split(", ").map(_.toInt)
      accum + (if (numberOfRegistersFor(instruction, before, after) >= 3) 1 else 0)
    })
  }

  def numberOfRegistersFor(instruction: Array[Int], before: Array[Int], after: Array[Int]): Int = {
    var totalSet = mutable.HashSet[String]()

    //addr
    if (after(instruction(3)) == before(instruction(1)) + before(instruction(2))) totalSet = totalSet + "addr"
    //addi
    if (after(instruction(3)) == before(instruction(1)) + instruction(2)) totalSet = totalSet + "addi"

    //mulr
    if (after(instruction(3)) == before(instruction(1)) * before(instruction(2))) totalSet = totalSet + "mulr"
    //muli
    if (after(instruction(3)) == before(instruction(1)) * instruction(2)) totalSet = totalSet + "muli"

    //banr
    if (after(instruction(3)) == (before(instruction(1)) & before(instruction(2)))) totalSet = totalSet + "banr"
    //bani
    if (after(instruction(3)) == (before(instruction(1)) & instruction(2))) totalSet = totalSet + "bani"

    //borr
    if (after(instruction(3)) == (before(instruction(1)) | before(instruction(2)))) totalSet = totalSet + "borr"
    //bori
    if (after(instruction(3)) == (before(instruction(1)) | instruction(2))) totalSet = totalSet + "bori"

    //setr
    if (after(instruction(3)) == before(instruction(1))) totalSet = totalSet + "setr"
    //seti
    if (after(instruction(3)) == instruction(1)) totalSet = totalSet + "seti"

    //gtir
    if (after(instruction(3)) == (if (instruction(1) > before(instruction(2))) 1 else 0)) totalSet = totalSet + "gtir"
    //gtri
    if (after(instruction(3)) == (if (before(instruction(1)) > instruction(2)) 1 else 0)) totalSet = totalSet + "gtri"
    //gtrr
    if (after(instruction(3)) == (if (before(instruction(1)) > before(instruction(2))) 1 else 0)) totalSet = totalSet + "gtrr"

    //eqir
    if (after(instruction(3)) == (if (instruction(1) == before(instruction(2))) 1 else 0)) totalSet = totalSet + "eqir"
    //eqri
    if (after(instruction(3)) == (if (before(instruction(1)) == instruction(2)) 1 else 0)) totalSet = totalSet + "eqri"
    //eqrr
    if (after(instruction(3)) == (if (before(instruction(1)) == before(instruction(2))) 1 else 0)) totalSet = totalSet + "eqrr"

//    println(s"${instruction(0)} = ${totalSet.mkString(" ")}")

    totalSet.size
  }

  override protected def B(input: String): Any = {
    var lastReg = Array[Int](0,0,0,0)
    input.split("\n").zipWithIndex.filter(lines => lines._2 > 3093).foreach(line => {
      lastReg = calculate(line._1.split(" ").map(_.toInt), lastReg)
    })
    lastReg(0)
  }

  def calculate(instruction: Array[Int], before: Array[Int]): Array[Int] = {
    var toReturn = before

    if (instruction(0) == 0) { //gtrr
      toReturn(instruction(3)) = if (before(instruction(1)) > before(instruction(2))) 1 else 0
    }
    else if (instruction(0) == 1) { //borr
      toReturn(instruction(3)) = before(instruction(1)) | before(instruction(2))
    }
    else if (instruction(0) == 2) { //gtir
      toReturn(instruction(3)) = if (instruction(1) > before(instruction(2))) 1 else 0
    }
    else if (instruction(0) == 3) { //eqri
      toReturn(instruction(3)) = if (before(instruction(1)) == instruction(2)) 1 else 0
    }
    else if (instruction(0) == 4) { //addr
      toReturn(instruction(3)) = before(instruction(1)) + before(instruction(2))
    }
    else if (instruction(0) == 5) { // seti
      toReturn(instruction(3)) = instruction(1)
    }
    else if (instruction(0) == 6) { //eqrr
      toReturn(instruction(3)) = if (before(instruction(1)) == before(instruction(2))) 1 else 0
    }
    else if (instruction(0) == 7) { //gtri
      toReturn(instruction(3)) = if (before(instruction(1)) > instruction(2)) 1 else 0
    }
    else if (instruction(0) == 8) { //banr
      toReturn(instruction(3)) = before(instruction(1)) & before(instruction(2))
    }
    else if (instruction(0) == 9) { //addi
      toReturn(instruction(3)) = before(instruction(1)) + instruction(2)
    }
    else if (instruction(0) == 10) { //setr
      toReturn(instruction(3)) = before(instruction(1))
    }
    else if (instruction(0) == 11) { //mulr
      toReturn(instruction(3)) = before(instruction(1)) * before(instruction(2))
    }
    else if (instruction(0) == 12) { //bori
      toReturn(instruction(3)) = before(instruction(1)) | instruction(2)
    }
    else if (instruction(0) == 13) { //muli
      toReturn(instruction(3)) = before(instruction(1)) * instruction(2)
    }
    else if (instruction(0) == 14) { //eqir
      toReturn(instruction(3)) = if (instruction(1) == before(instruction(2))) 1 else 0
    }
    else if (instruction(0) == 15) { //bani
      toReturn(instruction(3)) = before(instruction(1)) & instruction(2)
    }
    toReturn
  }

  override protected def test(): Unit = {
    assert(A("Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]\n")==1)
  }
}
