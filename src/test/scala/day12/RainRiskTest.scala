package day12

import utils.BaseTestSuite
import day12.RainRisk._
import utils.InputUtils.read

class RainRiskTest extends BaseTestSuite {

  val example =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin.split("\n").toList

  "Rain Risk" should "parse instructions" in {
    parseInstructions(example) shouldBe List(
      Instruction("F", 10),
      Instruction("N", 3),
      Instruction("F", 7),
      Instruction("R", 90),
      Instruction("F", 11)
    )
  }

  it should "move ship (example)" in {
    val data = parseInstructions(example)
    moveShip(data) shouldBe Pos(17, -8, -90)
  }

  it should "get Manhattan distance (example)" in {
    computeFinaleManhattanDistance(example) shouldBe 25
  }

  it should "get Manhattan distance - part 1" in {
    computeFinaleManhattanDistance(read("day12.txt")) shouldBe 1133
  }

  it should "rotate waypoint" in {
    WayPoint(0,0, 10, 4).rotate(-90) shouldBe WayPoint(0, 0, 4, -10)
    WayPoint(0,0, 10, 4).rotate(90) shouldBe WayPoint(0, 0, -4, 10)
    WayPoint(0,0, 10, 4).rotate(270) shouldBe WayPoint(0, 0, 4, -10)

    WayPoint(0,0, -4, 10).rotate(90) shouldBe WayPoint(0, 0, -10, -4)
    WayPoint(0,0, -4, 10).rotate(180) shouldBe WayPoint(0, 0, 4, -10)
    WayPoint(0,0, -4, 10).rotate(-180) shouldBe WayPoint(0, 0, 4, -10)
    WayPoint(0,0, -4, 10).rotate(270) shouldBe WayPoint(0, 0, 10, 4)
  }


  it should "move ship with waypoint (example)" in {
    val data = parseInstructions(example)
    moveShipWayPoint(data) shouldBe WayPoint(214, -72, 4, -10)
  }


  it should "get Manhattan distance - part 2" in {
    computeFinaleManhattanDistanceWayPoint(read("day12.txt")) shouldBe 61053
  }
}
