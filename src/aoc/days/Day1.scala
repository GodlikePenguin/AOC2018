package aoc.days

import java.util

import aoc.Day

import scala.collection.mutable

object Day1 extends Day(1) {
  override def A(input: String): Int = input.split("\n").foldLeft(0)((accum, item)=> accum + Integer.parseInt(item))

  override def B(input: String) : Int = {
    val freqs = new mutable.HashSet[Int]()
    var sum = 0
    freqs.add(sum)
    val nums = input.split("\n")
    while(true) {
      for (num <- nums) {
        sum += Integer.parseInt(num)
        if (freqs.contains(sum)) {
          return sum
        }
        freqs.add(sum)
      }
    }
    0
  }

  override def test(): Unit = {
    assert(A("+1\n+1\n+1")==3)
    assert(B("+1\n-1")==0)
  }
}
