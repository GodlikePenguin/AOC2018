package days

object Day1 {
  def run(input: String): String = input.reverse

  def test(): Unit = assert(run("hello") == "olleh", "The reverse of 'hello' should be 'olleh'")
}
