package aoc.days

import aoc.Day

import scala.collection.mutable
import scala.util.control.Breaks

object Day21 extends Day(21) {
  override protected def A(input: String): Any = {
    val lines = input.split("\n")
    val IR = lines(0).substring(4).toInt
    val instructions = lines.takeRight(lines.length-1)
    var registers = Array[Int](0,0,0,0,0,0)
    Breaks.breakable {
      while (registers(IR) < instructions.length) {
        registers = calculateNewRegisters(instructions(registers(IR)), registers)
        registers(IR) = registers(IR) + 1
        if (registers(IR) == 28) {
          Breaks.break
        }
      }
    }
    registers(5)
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
    else if (instructionString.contains("eqri")) { //eqri
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
    val lines = input.split("\n")
    val IR = lines(0).substring(4).toInt
    val instructions = lines.takeRight(lines.length-1)
    val r5Vals = new mutable.HashSet[Int]()
    var lastr5 = 0
    var registers = Array[Int](0,0,0,0,0,0)
    Breaks.breakable {
      while (registers(IR) < instructions.length) {
        registers = calculateNewRegisters(instructions(registers(IR)), registers)
        registers(IR) = registers(IR) + 1
        if (registers(IR) == 28) {
          if (registers(5) != lastr5) {
            if (r5Vals.contains(registers(5))) {
              Breaks.break
            }
            r5Vals.add(registers(5))
            lastr5 = registers(5)
          }
        }
      }
    }
    lastr5
  }

  override protected def test(): Unit = {}
}
