package aoc.days

import aoc.Day

object Day1 extends Day(1) {
  override def A(input: String): Int = {
    val nums = input.toCharArray.map(_.asDigit)
    nums.indices.map(i => if (nums(i) == nums((i+1)%nums.length)) nums(i) else 0).sum
  }

  override def B(input: String): Int = {
    val nums = input.toCharArray.map(_.asDigit)
    nums.indices.map(i => if (nums(i) == nums((i+nums.length/2)%nums.length)) nums(i) else 0).sum
  }

  override def test(): Unit = {
    assert(A("1122")==3)
    assert(B("1212")==6)
  }
}
