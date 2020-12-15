package day15

import utils.BaseTestSuite
import day15.RambunctiousRecitation._


class RambunctiousRecitationTest extends BaseTestSuite {

  val example = List(0,3,6)

  "Rambunctious Recitation" should "find first ten items" in {
    generate(example, 10) shouldBe List(0,3,6,0,3,3,1,0,4,0)
  }

  it should "compute 2020th element (example)" in {
    //generate(example, 2020).last shouldBe 436
   // generate(List(1,3,2), 2020).last shouldBe 1
    generate(List(3,1,2), 2020).last shouldBe 1836
  }

  it should "compute 2020th element - part 1" in {
    generate(List(18,11,9,0,5,1), 2020).last shouldBe 959
  }

  it should "compute 30e6th element (example)" in {
    generate(example, 30000000).last shouldBe 175594
  }

}
