package day22

import utils.BaseTestSuite
import day22.Combat._
import utils.InputUtils.read

import scala.collection.immutable.Queue

class CombatTest extends BaseTestSuite {

  val example =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10""".stripMargin.split("\n").toList


  "Combat" should "parse the cards" in {
    parse(example) shouldBe (
      Queue(9,2,6,3,1),
      Queue(5,8,4,7,10)
    )
  }

  it should "get the final winning deck" in {
    val res = play(parse(example))
    res shouldBe Queue(3, 2, 10, 6, 8, 5, 9, 4, 7, 1)
  }

  it should "compute final score (example)" in {
    val res = score(play(parse(example)))
    res shouldBe 306
  }

  it should "compute final score - part 1" in {
    val res = score(play(parse(read("day22.txt"))))
    res shouldBe 32629
  }


  it should "check for infinite Loop" in {
    checkInfiniteLoop(Player(deck = Queue(9,2,6,3,1),
      history = Set("1,2,3,4,5", "9,2,6,3,1"))) shouldBe true

    checkInfiniteLoop(Player(deck = Queue(9,2,6,3,1),
      history = Set("1,2,3,4,5", "9,2,6,3,8"))) shouldBe false
  }

  it should "play recursive combat (example)" in {
    val res = playRecursiveCombat(parse(example), display = true)
    res shouldBe Queue(7, 5, 6, 2, 4, 1, 10, 8, 9, 3)
    score(res) shouldBe 291
  }

  it should "play recursive combat - part 2" in {
    val res = playRecursiveCombat(parse(read("day22.txt")))
    score(res) shouldBe 32519
  }
}
