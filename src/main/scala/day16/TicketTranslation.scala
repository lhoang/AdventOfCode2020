package day16

import utils.InputUtils.splitInput

object TicketTranslation {

  type Ticket = Array[Int]

  case class Rule(name: String, ranges: Seq[Range])

  case class Input(rules: List[Rule], myTicket: Ticket, nearbyTickets: List[Ticket])

  def parseInput(input: List[String]): Input = {
    val parts = splitInput(input)

    def parseTicket(line: String): Array[Int] = line.split(",").map(_.toInt)

    val rules = parseRules(parts(0))
    val myTicket = parseTicket(parts(1).last)
    val nearbyTickets = parts(2).drop(1).map(parseTicket)
    Input(rules, myTicket, nearbyTickets)
  }

  def parseRules(input: List[String]): List[Rule] = {
    val regex = """([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r
    input.map {
      case regex(name, a, b, c, d) =>
        Rule(name, Seq(a.toInt to b.toInt, c.toInt to d.toInt))
    }
  }

  def errorRate(rules: List[Rule], tickets: List[Ticket]): Int = {
    val ranges = rules.flatMap(rule => rule.ranges).flatten.toSet
    tickets.flatMap(_.filterNot(ranges(_))).sum
  }

  def cleanTickets(rules: List[Rule], tickets: List[Ticket]): List[Ticket] = {
    val ranges = rules.flatMap(rule => rule.ranges).flatten.toSet
    tickets.filter(_.forall(ranges(_)))
  }

  def identifyColumns(rules: List[Rule], tickets: List[Ticket]): Map[String, Int] = {
    val ranges = rules.map(r => (r.name, r.ranges.flatten.toSet))

    def findNames(values: List[Int]): List[String] =
      ranges.filter { case (_, range) => values.forall(range(_)) }
        .map { case (name, _) => name }

    val possibleNames: List[(List[String], Int)] = tickets.transpose
      .map(findNames)
      .zipWithIndex

    def selectName(remainingColumns: List[(List[String], Int)], namesMap: Map[String, Int]): Map[String, Int] = {
      if (remainingColumns.isEmpty) namesMap
      else {
        val (singles, others) = remainingColumns.partition { case (fields, _) => fields.size == 1 }
        val newNames = namesMap ++ singles.map { case (fields, index) => fields.head -> index }
        val remaining = others.map { case (fields, index) => fields.filterNot(namesMap.contains) -> index }
        selectName(remaining, newNames)
      }
    }
    selectName(possibleNames, Map.empty[String, Int])
  }

  def getValuesDepartures(ticket: Ticket, columns: Map[String, Int]): Long =
    columns.filter { case (name, _) => name.startsWith("departure") }
      .map { case (_, i) => ticket(i).toLong }
      .product

}
