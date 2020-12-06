package utils

import utils.InputUtils._

class InputUtilsTest extends BaseTestSuite {

  val example =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin.split("\n").toList

  "Input Utils" should "read all the lines" in {
    val lines = read("test.txt")
    lines should contain theSameElementsInOrderAs Seq("Hello", "Goodbye")
  }

  it should "split the input" in {
    val res = splitInput(example)
    res should have size 5
    res(0) shouldBe List("abc")
    res(1) shouldBe List("a", "b", "c")
  }
}
