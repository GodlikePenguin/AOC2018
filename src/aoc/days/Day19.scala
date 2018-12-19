package aoc.days

import aoc.Day

object Day19 extends Day(19) {
  override protected def A(input: String): Any = {
    val lines = input.split("\n")
    val IR = lines(0).substring(4).toInt
    val instructions = lines.takeRight(lines.length-1)
    var registers = Array[Int](0,0,0,0,0,0)
    while (registers(IR) < instructions.length) {
      registers = calculateNewRegisters(instructions(registers(IR)), registers)
      registers(IR) = registers(IR) + 1
    }
    registers(0)
  }

  def calculateNewRegisters(instructionString: String, before: Array[Int]): Array[Int] = {
    var toReturn = before
    val temp = instructionString.split(" ")
    temp(0) = "0"
    val instruction = temp.map(_.toInt)

    if (instructionString.contains("gtrr")) { //gtrr
      toReturn(instruction(3)) = if (before(instruction(1)) > before(instruction(2))) 1 else 0
    }
    else if (instructionString.contains("borr")) { //borr
      toReturn(instruction(3)) = before(instruction(1)) | before(instruction(2))
    }
    else if (instructionString.contains("gtir")) { //gtir
      toReturn(instruction(3)) = if (instruction(1) > before(instruction(2))) 1 else 0
    }
    else if (instructionString.contains("egri")) { //eqri
      toReturn(instruction(3)) = if (before(instruction(1)) == instruction(2)) 1 else 0
    }
    else if (instructionString.contains("addr")) { //addr
      toReturn(instruction(3)) = before(instruction(1)) + before(instruction(2))
    }
    else if (instructionString.contains("seti")) { // seti
      toReturn(instruction(3)) = instruction(1)
    }
    else if (instructionString.contains("eqrr")) { //eqrr
      toReturn(instruction(3)) = if (before(instruction(1)) == before(instruction(2))) 1 else 0
    }
    else if (instructionString.contains("gtri")) { //gtri
      toReturn(instruction(3)) = if (before(instruction(1)) > instruction(2)) 1 else 0
    }
    else if (instructionString.contains("banr")) { //banr
      toReturn(instruction(3)) = before(instruction(1)) & before(instruction(2))
    }
    else if (instructionString.contains("addi")) { //addi
      toReturn(instruction(3)) = before(instruction(1)) + instruction(2)
    }
    else if (instructionString.contains("setr")) { //setr
      toReturn(instruction(3)) = before(instruction(1))
    }
    else if (instructionString.contains("mulr")) { //mulr
      toReturn(instruction(3)) = before(instruction(1)) * before(instruction(2))
    }
    else if (instructionString.contains("bori")) { //bori
      toReturn(instruction(3)) = before(instruction(1)) | instruction(2)
    }
    else if (instructionString.contains("muli")) { //muli
      toReturn(instruction(3)) = before(instruction(1)) * instruction(2)
    }
    else if (instructionString.contains("egir")) { //eqir
      toReturn(instruction(3)) = if (instruction(1) == before(instruction(2))) 1 else 0
    }
    else if (instructionString.contains("bani")) { //bani
      toReturn(instruction(3)) = before(instruction(1)) & instruction(2)
    } else {
      throw new Exception("Unrecognised instruction "+ instructionString)
    }
    toReturn
  }

  override protected def B(input: String): Any = {
    val maxD = Math.round(Math.sqrt(10551398L))
    var sum=1L
    for(i <- 2L to maxD) {
      if(10551398 % i == 0) {
        sum += i
        val d = 10551398L/i
        if(d!=i) sum+=d
      }
    }
    sum + 10551398L
  }

  override protected def test(): Unit = {
    assert(A("#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5")==7)
  }
}
