package day07

object Haversacks {

  type Rules = Map[String, Map[String, Int]]

  def parseRule(s: String): (String, Map[String, Int]) = {
    val ruleBags = """([\w ]+) bags contain (.*)\.""".r
    val colorBags = """(\d+) ([\w ]+) bags?""".r
    val ruleNoBag = """([\w ]+) bags contain no other bags\.""".r

    s match {
      case ruleNoBag(color) => color -> Map.empty[String, Int]
      case ruleBags(color, tail) =>
        color -> tail.split(", ").map {
          case colorBags(count, bColor) => bColor -> count.toInt
        }.toMap
    }
  }

  def parseRules(input: List[String]): Rules = input.map(parseRule).toMap

  def containsShinyGoldBags(color: String, rules: Rules): Boolean = {
    def rec(current: String): Boolean = {
      val subBags = rules.getOrElse(current, Map.empty)
      if (subBags.isEmpty) false
      else if (subBags.keys.toSeq.contains("shiny gold")) true
      else subBags.keys.exists(rec)
    }
    rec(color)
  }

  def countColorsContainingShinyGoldBags(rules: Rules): Int =
    rules.keys.count(color => containsShinyGoldBags(color, rules))

  def countBagsInside(rules: Rules): Int = {
    def rec(current: String): Int = {
      val subBags = rules.getOrElse(current, Map.empty)
      if (subBags.isEmpty) 1
      else subBags
        .map { case (color, count) => count * rec(color) }
        .sum + 1 // count the current bag
    }
    rec("shiny gold") - 1 // don't count the shiny gold bag
  }

}
