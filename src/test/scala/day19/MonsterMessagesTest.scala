package day19

import utils.BaseTestSuite
import day19.MonsterMessages._
import utils.InputUtils.read

class MonsterMessagesTest extends BaseTestSuite {

  val example =
    """0: 4 1 5
      |1: 2 3 | 3 2
      |2: 4 4 | 5 5
      |3: 4 5 | 5 4
      |4: "a"
      |5: "b"""".stripMargin.split("\n").toList

  val messages =
    """ababbb
      |bababa
      |abbbab
      |aaabbb
      |aaaabbb""".stripMargin.split("\n").toList

  val regexp = "a( (aa|bb)(ab|ba) | (ab|ba)(aa|bb) )b".replaceAll(" ", "")

  "Monster Messages" should "parse rules" in {
    parseRules(example) shouldBe regexp
  }

  it should "the right regex (example)" in {
    val r = regexp.r
    messages.map(m => r.matches(m)) shouldBe List(true, false, true, false, false)
  }

  it should "count the valid messages (example)" in {
    val input = example++(""::messages)
    countValidMessages(input) shouldBe 2
  }

  it should "count the valid messages - Part 1" in {
    countValidMessages(read("day19.txt")) shouldBe 285
  }

  it should "count the valid messages - Part 2, Example" in {
    countValidMessages(read("day19-example.txt")) shouldBe 3
  }

  it should "validate repeat pattern" in {
    val p42 = "((b(a(bb|ab)|b(a|b)(a|b))|a(bbb|a(bb|a(a|b))))b|(((aa|ab)a|bbb)b|((a|b)a|bb)aa)a)"
    val p8 = s"($p42){1,}".r

    List("bbaab", "bbaabbbaab", "aaababbaab")
      .map(m => p8.matches(m)) shouldBe List(true, true, true)
  }

  it should "validate inside repeat pattern" in {
    val p42 = "aaa"
    val p31 = "bbb"
    val p11 = s"($p42){1,}($p31){1,}".r

    List("aaabbb", "aaaaaabbbbbb", "aaabbbbbb")
      .map(m => p11.matches(m)) shouldBe List(true, true, true)
  }

  it should "count the valid messages 2, bigger example Fixed" in {
    countValidMessagesFixed(read("day19-example_fixed.txt")) shouldBe 12
  }

  it should "count the valid messages - Part 2" in {
    countValidMessagesFixed(read("day19_fixed.txt")) shouldBe 412
  }

}
