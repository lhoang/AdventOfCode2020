package day11

import utils.BaseTestSuite
import day11.SeatingSystem._
import utils.InputUtils.read

class SeatingSystemTest extends BaseTestSuite {

  val exampleStart =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin.split("\n").toList

  val exampleStep1 =
    """#.##.##.##
      |#######.##
      |#.#.#..#..
      |####.##.##
      |#.##.##.##
      |#.#####.##
      |..#.#.....
      |##########
      |#.######.#
      |#.#####.##""".stripMargin.split("\n").toList

  val exampleStep2 =
    """#.LL.L#.##
      |#LLLLLL.L#
      |L.L.L..L..
      |#LLL.LL.L#
      |#.LL.LL.LL
      |#.LLLL#.##
      |..L.L.....
      |#LLLLLLLL#
      |#.LLLLLL.L
      |#.#LLLL.##""".stripMargin.split("\n").toList

  val exampleStep2Visible =
    """#.LL.LL.L#
      |#LLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLL#
      |#.LLLLLL.L
      |#.LLLLL.L#""".stripMargin.split("\n").toList


  "Seating System" should "count occupied seats" in {
    countAdjacentOccupiedSeats(1,1, exampleStep2) shouldBe 2
    countAdjacentOccupiedSeats(1,0, exampleStep2) shouldBe 2
    countAdjacentOccupiedSeats(6,5, exampleStep2) shouldBe 0
    countAdjacentOccupiedSeats(6,6, exampleStep2) shouldBe 1
    countAdjacentOccupiedSeats(0,6, exampleStep1) shouldBe 3
  }

  it should "play round" in {
    displayRoom(exampleStart)
    val round1 = playRoundAdjacent(exampleStart, display = true)
    val round2 = playRoundAdjacent(round1, display = true)
    round2 shouldBe exampleStep2
  }

  it should "compare 2 rooms" in {
    val round1 = playRoundAdjacent(exampleStart)
    (round1 == exampleStep1) shouldBe true
    (round1 == exampleStep2) shouldBe false
  }

  it should "count adjacent occupied seat when stabilized (example)" in {
    stabilizeAndCountAdjacent(exampleStart, display = true) shouldBe 37
  }

  it should "count adjacent occupied seat when stabilized - Part 1" in {
    stabilizeAndCountAdjacent(read("Day11.txt")) shouldBe 2468
  }

  it should "count visible occupied seat" in {
    val ex1 =
      """.......#.
        |...#.....
        |.#.......
        |.........
        |..#L....#
        |....#....
        |.........
        |#........
        |...#.....
        |
        |""".stripMargin.split("\n").toList

    val ex2 =
      """.............
        |.L.L.#.#.#.#.
        |.............""".stripMargin.split("\n").toList

    val ex3 =
      """.##.##.
        |#.#.#.#
        |##...##
        |...L...
        |##...##
        |#.#.#.#
        |.##.##.""".stripMargin.split("\n").toList

    countVisibleOccupiedSeats(3, 4, ex1) shouldBe 8

    countVisibleOccupiedSeats(1, 1, ex2) shouldBe 0
    countVisibleOccupiedSeats(3, 1, ex2) shouldBe 1

    countVisibleOccupiedSeats(3, 3, ex3) shouldBe 0
  }

  it should "play round visible" in {
    displayRoom(exampleStart)
    val round1 = playRoundVisible(exampleStart, display = true)
    val round2 = playRoundVisible(round1, display = true)
    round2 shouldBe exampleStep2Visible
  }


  it should "count visible occupied seat when stabilized (example)" in {
    stabilizeAndCountVisible(exampleStart, display = true) shouldBe 26
  }

  it should "count visible occupied seat when stabilized - Part 2" in {
    stabilizeAndCountVisible(read("Day11.txt")) shouldBe 2214
  }
}
