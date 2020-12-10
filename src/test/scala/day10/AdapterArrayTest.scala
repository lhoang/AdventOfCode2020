package day10

import utils.BaseTestSuite
import day10.AdapterArray._
import org.scalatest.PartialFunctionValues._
import utils.InputUtils.read

class AdapterArrayTest extends BaseTestSuite {

  val example1 = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
  val example2 = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45,
    19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)

  "Adapter Array" should "compute diff (examples)" in {
    val res = computeDiff(example1)
    res.valueAt(1) shouldBe 7
    res.valueAt(3) shouldBe 5

    val res2 = computeDiff(example2)
    res2.valueAt(1) shouldBe 22
    res2.valueAt(3) shouldBe 10
  }

  it should "compute diff - Part 1" in {
    val res = computeDiff(parse(read("day10.txt")))
    res.valueAt(1) shouldBe 65
    res.valueAt(3) shouldBe 34

    println(res.valueAt(1) * res.valueAt(3))
  }

  it should "split List" in {
    val x = List(1, 2, 3, 6, 7, 10, 12, 14, 15)
    splitInput(x) shouldBe List(List(1,2,3), List(6,7), List(10), List(12), List(14, 15))
  }

  it should "count combinations (example)" in  {
    countCombinations(example1) shouldBe 8
    countCombinations(example2) shouldBe 19208
  }

  it should "count combinations - part 2" in  {
    countCombinations(parse(read("day10.txt"))) shouldBe 7086739046912L
  }


  it should "count combinations with path (example)" in  {
    countCombinationsWithPath(example1) shouldBe 8
    countCombinationsWithPath(example2) shouldBe 19208
  }

  it should "count combinations with path - Part 2" in  {
    countCombinationsWithPath(parse(read("day10.txt"))) shouldBe 7086739046912L
  }

}
