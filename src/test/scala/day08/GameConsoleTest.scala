package day08

import day08.GameConsole._
import utils.BaseTestSuite
import utils.InputUtils.read

class GameConsoleTest extends BaseTestSuite {

  val example =
  """nop +0
    |acc +1
    |jmp +4
    |acc +3
    |jmp -3
    |acc -99
    |acc +1
    |jmp -4
    |acc +6""".stripMargin.split("\n").toList


  "Game Console" should "parse instructions" in {
    val res = parseInstructions(example)
    res should have size 9
    res.head shouldBe Instruction("nop", 0)
    res(3) shouldBe Instruction("acc", 3)
    res(5) shouldBe Instruction("acc", -99)
  }

  it should "compute instructions (example)" in {
    val instructions = parseInstructions(example)
    compute(instructions) shouldBe (5, false)
  }

  it should "compute instructions - Part 1" in {
    val instructions = parseInstructions(read("day8.txt"))
    compute(instructions)._1 shouldBe 2003
  }

  it should "fix and compute instructions (example)" in {
    val instructions = parseInstructions(example)
    fixAndCompute(instructions) shouldBe 8
  }

  it should "fix and compute instructions - Part 2" in {
    val instructions = parseInstructions(read("day8.txt"))
    fixAndCompute(instructions) shouldBe 1984
  }
}
