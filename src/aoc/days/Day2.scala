package aoc.days

import aoc.Day

import scala.collection.mutable

object Day2 extends Day(2) {
  override protected def A(input: String): Int = {
    var twos = 0
    var threes = 0
    input.split("\n").foreach(word => {
      var twoAdded = false
      var threeAdded = false
      var map = new mutable.HashMap[Char,Int]()
      word.foreach(char => map.put(char, word.count(_==char)))
      map.foreach(e => {
        if (e._2 == 2 && !twoAdded) {
          twos += 1
          twoAdded = true
        } else if (e._2 == 3 && !threeAdded) {
          threes += 1
          threeAdded = true
        }
      })
    })
    twos * threes
  }

  override protected def B(input: String): String = {
    for (a <- input.split("\n")) {
      for (b <- input.split("\n")) {
        val diff = Hamming.compute(a, b)
        if (diff == 1) {
          val inter = a intersect b
          return inter
        }
      }
    }
    ""
  }

  override protected def test(): Unit = {
    assert(A("abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab")==12)
    assert(B("abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz")=="fgij")
  }
}

object Hamming {
  def compute(s1: String, s2: String): Int = {
    if (s1.length != s2.length) return -1
    s1.zip(s2).count(pair => pair._1 != pair._2)
  }
}



