package day07

import day07.Haversacks._
import utils.BaseTestSuite
import org.scalatest.PartialFunctionValues._
import utils.InputUtils.read

class HaversacksTest extends BaseTestSuite {

  val example =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin.split("\n").toList


  val example2 =
    """shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.""".stripMargin.split("\n").toList

  "Haversacks" should "parse rules" in {
    val rules = parseRules(example)
    rules should have size 9
    rules.valueAt("light red") shouldBe Map("bright white" -> 1, "muted yellow" -> 2)
    rules.valueAt("dark orange") shouldBe Map("bright white" -> 3, "muted yellow" -> 4)
    rules.valueAt("bright white") shouldBe Map("shiny gold" -> 1)
    rules.valueAt("faded blue") shouldBe Map.empty
  }

  it should "parse rule" in {
    val rule = "muted bronze bags contain 5 bright tomato bags, 5 light red bags, 2 shiny yellow bags, 2 dim teal bags."
    parseRule(rule) shouldBe("muted bronze", Map(
      "bright tomato" -> 5,
      "light red" -> 5,
      "shiny yellow" -> 2,
      "dim teal" -> 2
    ))
  }

  it should "find bags containing shiny gold bags" in {
    val rules = parseRules(example)
    containsShinyGoldBags("light red", rules) shouldBe true
    containsShinyGoldBags("muted yellow", rules) shouldBe true
    containsShinyGoldBags("dark orange", rules) shouldBe true
    containsShinyGoldBags("light red", rules) shouldBe true

    containsShinyGoldBags("dark olive", rules) shouldBe false
    containsShinyGoldBags("vibrant plum", rules) shouldBe false
    containsShinyGoldBags("faded blue", rules) shouldBe false
  }

  it should "find colors containing shiny gold bags (example)" in {
    val rules = parseRules(example)
    countColorsContainingShinyGoldBags(rules) shouldBe 4
  }

  it should "find colors containing shiny gold bags - Part 1" in {
    val rules = parseRules(read("day7.txt"))
    countColorsContainingShinyGoldBags(rules) shouldBe 261
  }

  it should "count bags inside shiny gold (example)" in {
    val rules = parseRules(example)
    countBagsInside(rules) shouldBe 32

    val rules2 = parseRules(example2)
    countBagsInside(rules2) shouldBe 126
  }

  it should "count bags inside shiny gold - Part 2" in {
    val rules = parseRules(read("day7.txt"))
    countBagsInside(rules) shouldBe 3765
  }
}
