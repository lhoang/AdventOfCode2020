package day23

import utils.BaseTestSuite
import day23.CrabCups._

class CrabCupsTest extends BaseTestSuite {

  val input = "394618527"
  val example = "389125467"

  "Cups" should "find destination cup" in {
    findDestinationCup(3, Seq(1,8,4)) shouldBe 2
    findDestinationCup(3, Seq(7,8,2)) shouldBe 1
    findDestinationCup(3, Seq(1,8,2)) shouldBe 9
    findDestinationCup(3, Seq(1,9,2)) shouldBe 8
    findDestinationCup(1, Seq(3,6,7)) shouldBe 9
    findDestinationCup(1, Seq(3,6,1_000_000), max = 1_000_000) shouldBe 999_999
  }

  it should "play rounds (example)" in {
    val round1 = playRound(example)
    round1 shouldBe "289154673"

    val round2 = playRound(round1)
    round2 shouldBe "546789132"

    val round6 = playRound("136792584")
    round6 shouldBe "936725841"
  }

  it should "play n rounds (example)" in {
    play(example, 3) shouldBe "891346725"
    play(example, 4) shouldBe "467913258"
    play(example, 5) shouldBe "136792584"
    play(example, 6) shouldBe "936725841"
    play(example, 7) shouldBe "258367419"
    play(example, 10) shouldBe "837419265"
    play(example, 100) shouldBe "167384529"
  }

  it should "get seq from 1" in {
    getFrom1("837419265") shouldBe "92658374"
  }

  it should "get seq from 1 after n rounds (example)" in {
    getFrom1(play(example, 100)) shouldBe "67384529"
  }

  it should "get seq from 1 after n rounds - part 1" in {
    getFrom1(play(input, 100)) shouldBe "78569234"
  }

  it should "play rounds for real (example)" in {
    val (round1, _) = realPlay(example, nCups = 9, rounds = 1)
    readFirstValues(round1, 9).mkString shouldBe "289154673"

    val (round10, _) = realPlay(example, nCups = 9, rounds = 10)
    readFirstValues(round10, 9).mkString shouldBe "837419265"

    val (round100, _) = realPlay(example, nCups = 9, rounds = 100)
    readFirstValues(round100, 9).mkString shouldBe "167384529"

    val (n10Cups, _) = realPlay(example, nCups = 15, rounds = 1)
    readFirstValues(n10Cups, 15).mkString shouldBe "289154671011121314153"
  }

  it should "find cups containing stars (example)" in {
    val (_, cup1) = realPlay(example, nCups = 1_000_000, rounds = 10_000_000)
    findProductOf2StarContainingCups(cup1) shouldBe 149245887792L
  }

  it should "find cups containing stars - part 2" in {
    val (_, cup1) = realPlay(input, nCups = 1_000_000, rounds = 10_000_000)
    findProductOf2StarContainingCups(cup1) shouldBe 565615814504L
  }

}
