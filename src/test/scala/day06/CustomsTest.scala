package day06

import day06.Customs._
import utils.BaseTestSuite
import utils.InputUtils.read

class CustomsTest extends BaseTestSuite {

  val example =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin.split("\n").toList

  "Customs" should "count all answers (example) " in {
    countDistinctAnswers(example) shouldBe 11
  }

  it should "count distinct answers - Part 1" in {
    countDistinctAnswers(read("day6.txt")) shouldBe 6382
  }

  it should "count answers with yes for everyone (example)" in {
    countAllYesAnswers(example) shouldBe 6
  }

  it should "count answers with yes for everyone - Part 2" in {
    countAllYesAnswers(read("day6.txt")) shouldBe 3197
  }

}
