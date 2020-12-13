package day13

import utils.BaseTestSuite
import day13.ShuttleSearch._
import utils.InputUtils.read

class ShuttleSearchTest extends BaseTestSuite {

  val example = List("939", "7,13,x,x,59,x,31,19")

  "Shuttle Search" should "find next shuttle (example)" in {
    val res1 = findNextShuttle(example)
    res1 shouldBe (5, 59)
    getAnswer(res1) shouldBe 295

    findNextShuttle(List("938", "7,13,x,x,59,x,31,19")) shouldBe (0,7)
  }

  it should  "find next shuttle - part 1" in {
    getAnswer(findNextShuttle(read("day13.txt"))) shouldBe 3464
  }

  it should "build function that validate timestamp" in {
    val validate = validTs(getShuttlesWithIndex(example(1)))
    validate(1068781) shouldBe true
  }

  it should "find number giving modulo = 1" in {
    findMultipleMod1(BigInt(28), 3) shouldBe BigInt(28)
    findMultipleMod1(BigInt(12), 7) shouldBe BigInt(36)
    findMultipleMod1(BigInt(35), 3) shouldBe BigInt(70)
  }


  it should "find the first valid timestamp (example)" in {
    val shuttles = getShuttlesWithIndex(example(1))
    findValidTs(shuttles) shouldBe 1068781
    val sunZi = Array(
      3 -> 1,
      5 -> 2,
      7 -> 5
    )
    findValidTsBrutForce(sunZi) shouldBe 23

    val shuttles2 = getShuttlesWithIndex("3,4,x,7")
    findValidTs(shuttles2) shouldBe 39
  }

  it should "find the first valid timestamp - part 2" in {
    val shuttles = getShuttlesWithIndex(read("day13.txt")(1))
    findValidTs(shuttles) shouldBe BigInt("760171380521445")
  }
}
