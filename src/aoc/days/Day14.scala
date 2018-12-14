package aoc.days

import aoc.Day

import scala.collection.mutable.ArrayBuffer

object Day14 extends Day(14) {
  override protected def A(input: String): Any = {
    val iterations = input.toInt
    var recipes = new ArrayBuffer[Int]()
    recipes += 3
    recipes += 7
    var elf1Index = 0
    var elf2Index = 1
    while (recipes.length < iterations + 11) {
      var newRecipe = recipes(elf1Index) + recipes(elf2Index)
      var (tens, units) = split(newRecipe)
      if (tens != 0) recipes += tens
      recipes += units
      elf1Index = (elf1Index + 1 + recipes(elf1Index)) % recipes.length
      elf2Index = (elf2Index + 1 + recipes(elf2Index)) % recipes.length
    }
    var output = ""
    for (i <- iterations until iterations+10) {
      output += recipes(i).toString
    }
    output
  }

  def split(i: Int): (Int, Int) = if (i < 10) (0, i) else (1, i-10)

  override protected def B(input: String): Any = {
    val iterations = input.toInt
    var recipes = new ArrayBuffer[Int]()
    recipes += 3
    recipes += 7
    var elf1Index = 0
    var elf2Index = 1
    var found = false
    var lastAdded = ""
    while (!found) {
      var newRecipe = recipes(elf1Index) + recipes(elf2Index)
      var (tens, units) = split(newRecipe)
      if (tens != 0) recipes += tens
      recipes += units
      lastAdded = recipes.takeRight(input.length+1).foldLeft("")((a, b) => a + b.toString)
      if (lastAdded.contains(input)) {
        found = true
      }
      elf1Index = (elf1Index + 1 + recipes(elf1Index)) % recipes.length
      elf2Index = (elf2Index + 1 + recipes(elf2Index)) % recipes.length
    }
    if (recipes.takeRight(input.length).foldLeft("")((a, b) => a + b.toString) == input) {
      recipes.length - input.length
    }  else {
      (recipes.length - input.length) - 1
    }
  }

  override protected def test(): Unit = {
    assert(split(18)==(1,8))
    assert(split(0)==(0,0))
    assert(split(5)==(0,5))
    assert(A("9")=="5158916779")
    assert(A("5")=="0124515891")
    assert(A("18")=="9251071085")
    assert(A("2018")=="5941429882")
    assert(B("51589")==9)
    assert(B("01245")==5)
    assert(B("92510")==18)
    assert(B("59414")==2018)
  }
}
