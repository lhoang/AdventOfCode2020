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
}
