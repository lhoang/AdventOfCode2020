package day15

import utils.BaseTestSuite
import day15.RambunctiousRecitation._


class RambunctiousRecitationTest extends BaseTestSuite {

  val example = List(0,3,6)

  "Rambunctious Recitation" should "find first ten items" in {
    generate(example, 10) shouldBe List(0,3,6,0,3,3,1,0,4,0)
  }

  it should "compute 2020th element (example)" in {
    generate3(example, 2020) shouldBe 436
    //generate3(List(1,3,2), 2020) shouldBe 1
    //generate3(List(3,1,2), 2020) shouldBe 1836
    //generate3(List(3,1,2), 2020) shouldBe 1836
  }

  it should "compute 2020th element - part 1" in {
    generate3(List(18,11,9,0,5,1), 2020) shouldBe 959
  }

  it should "compute 30e6th element (example)" in {
    generate3(example, 30_000_000) shouldBe 175594
  }

  it should "compute 30e6th element - part 2" in {
    generate3(List(18,11,9,0,5,1), 30_000_000) shouldBe 116590
  }

}
