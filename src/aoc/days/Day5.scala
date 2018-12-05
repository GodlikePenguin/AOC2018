package aoc.days

import aoc.Day

import scala.util.control.Breaks

object Day5 extends Day(5){

  override protected def A(input: String): Int = {
    var currentLength = input.length
    var newInput = input
    do {
      currentLength = newInput.length
      Breaks.breakable {
        for (i <- 0 to newInput.length-2) {
          if (newInput.charAt(i).toString.equalsIgnoreCase(newInput.charAt(i+1).toString) && !newInput.charAt(i).equals(newInput.charAt(i+1))) {
            newInput = newInput.substring(0, i) + (if (i < newInput.length - 2) newInput.substring(i+2) else "")
            Breaks.break
          }
        }
      }

    } while (newInput.length != currentLength)
    newInput.length
  }

  protected def OldB(input: String): Any = {
    var currentMinimum = 1000000
    for (c <- 'a' to 'z') {
      val i = A(input.replaceAll(c.toString, "").replaceAll(c.toString.toUpperCase, ""))
      if (i < currentMinimum) {
        currentMinimum = i
      }
    }
    currentMinimum
  }

  override protected def B(input: String): Any = {
    val a = ('a' to 'z').par.map(c => {
      A(input.replaceAll(c.toString, "").replaceAll(c.toString.toUpperCase, ""))
    })
    a.min
  }

  override protected def test(): Unit = {
    assert(A("dabAcCaCBAcCcaDA")==10)
    assert(B("dabAcCaCBAcCcaDA")==4)
  }
}
