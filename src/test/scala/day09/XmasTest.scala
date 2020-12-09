package day09

import day09.Xmas._
import utils.BaseTestSuite
import utils.InputUtils.read

class XmasTest extends BaseTestSuite {

  val example =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin.split("\n").toList

  "Xmas" should "check Preamble" in {
    checkPreamble(62, List(35, 20, 15, 25, 47)) shouldBe true
    checkPreamble(63, List(35, 20, 15, 25, 47)) shouldBe false
  }

  it should "find weakness (example)" in {
    findWeakness(parse(example), preambleSize = 5) shouldBe 127
  }

  it should "find weakness - part 1" in {
    findWeakness(parse(read("day9.txt")), preambleSize = 25) shouldBe 248131121
  }

  it should "compute weakness (example)" in {
    computeWeakness(example, preambleSize = 5) shouldBe 62
  }

  it should "compute weakness - Part 2" in {
    computeWeakness(read("day9.txt"), preambleSize = 25) shouldBe 31580383
  }

}
