package aoc.days

import aoc.Day

import scala.collection.mutable

object Day2 extends Day(2) {
  override protected def A(input: String): Int = {
    var twos = 0
    var threes = 0
    input.split("\n").foreach(word => {
      var map = new mutable.HashMap[Char,Int]()
      word.foreach(char => map.put(char, word.count(_==char)))
      val set = map.values.toSet
      if (set.contains(2)) twos += 1
      if (set.contains(3)) threes += 1
    })
    twos * threes
  }

  override protected def B(input: String): String = {
    for (a <- input.split("\n")) {
      for (b <- input.split("\n")) {
        if (Hamming.compute(a, b) == 1) {
          return a intersect b
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



