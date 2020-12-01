package utils

import utils.InputUtils.read

class InputUtilsTest extends BaseTestSuite {

  "Input Utils" should "read all the lines" in {
    val lines = read("test.txt")
    lines should contain theSameElementsInOrderAs Seq("Hello", "Goodbye")
  }
}
