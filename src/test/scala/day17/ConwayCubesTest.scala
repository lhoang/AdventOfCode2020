package day17

import utils.BaseTestSuite
import day17.ConwayCubes._

class ConwayCubesTest extends BaseTestSuite {

  def s(a: String): List[String] = a.stripMargin.stripMargin.split("\n").toList

  val input =
    s(
      """..#..#..
        |#.#...#.
        |..#.....
        |##....##
        |#..#.###
        |.#..#...
        |###..#..
        |....#..#""")


  val example =
    s(
      """.#.
        |..#
        |###""")

  val cube = List(
    s(
      """...
        |...
        |..."""),
    s(
      """.#.
        |..#
        |###"""),
    s(
      """...
        |...
        |..."""),
  )


  "Conway Cube" should "count neighbours" in {

    countNeighbours(0, 0, 0, cube) shouldBe 1
    countNeighbours(1, 1, 1, cube) shouldBe 5
    countNeighbours(2, 2, 1, cube) shouldBe 2
  }

  it should "build a bigger cube" in {
    val res = biggerCube(cube)
    res should have size 5
    res.head should have size 5
    res(2)(3)(3) shouldBe '#'
    //displayCube(res)

    val flatRes = biggerCube(from2dTo3d(example))
    flatRes should have size 5
    flatRes.head should have size 5
    flatRes(2)(3)(3) shouldBe '#'
    displayCube(flatRes)
  }

  it should "play round (example)" in {
    val res = playRound(from2dTo3d(example), display = true)
    res(1) shouldBe s(""".....
                        |.....
                        |.#...
                        |...#.
                        |..#..""".stripMargin)
    res(2) shouldBe s(""".....
                        |.....
                        |.#.#.
                        |..##.
                        |..#..""".stripMargin)
    res(3) shouldBe s(""".....
                        |.....
                        |.#...
                        |...#.
                        |..#..""".stripMargin)
    val round2 = playRound(res)
    val round3 = playRound(round2, display = true)
  }

  it should "count actives cells (example)" in {
    val res = runRounds(example, 6)
    countActives(res) shouldBe 112
  }

  it should "count actives cells - part 1" in {
    val res = runRounds(input, 6)
    countActives(res) shouldBe 215
  }

  it should "build a bigger hypercube" in {
    val flatRes = biggerHCube(from2dTo4d(example))
    flatRes should have size 5
    flatRes.head should have size 5
    flatRes.head.head should have size 5
    flatRes(2)(2)(3)(3) shouldBe '#'
    //displayHCube(flatRes)

    val res8 = biggerHCube(from2dTo4d(input))
    res8 should have size 10
    res8.head should have size 10
    res8.head.head should have size 10
    displayHCube(res8)
  }

  it should "play Hround (example)" in {
    val res = playHRound(from2dTo4d(example), display = true)
    val round2 = playHRound(res, display = true)
    //val round3 = playRound(round2, display = true)
  }

  it should "count actives cells in Hypercube (example)" in {
    val res = runHRounds(example, 6, display = true)
    countHActives(res) shouldBe 848
  }

  it should "count actives cells in Hypercube - Part 2" in {
    val res = runHRounds(input, 6, display = false)
    countHActives(res) shouldBe 1728
  }
}
