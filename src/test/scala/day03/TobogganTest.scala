package day03

import day03.Toboggan._
import utils.BaseTestSuite
import utils.InputUtils.read

class TobogganTest extends BaseTestSuite {

    val example = List(
      "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    )

   val slopes = List(
     (1, 1),
     (3, 1),
     (5, 1),
     (7, 1),
     (1, 2)
   )

  "Toboggan" should "get the path" in {
    slide(example, right = 3, down = 1) shouldBe "..#.##.####"
  }

  it should "count trees (example)" in  {
    countTrees(example, right = 3, down = 1) shouldBe 7
  }

  it should "count trees - Part 1" in  {
    countTrees(read("day3.txt"), right = 3, down = 1) shouldBe 274L
  }

  it should "count the trees for all slopes (example)" in {
    countTreesSlopes(example, slopes) shouldBe 336L
  }

  it should "count the trees for all slopes - Part 2" in {
    countTreesSlopes(read("day3.txt"), slopes) shouldBe 6050183040L
  }
  
}
