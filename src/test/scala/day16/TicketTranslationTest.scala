package day16

import utils.BaseTestSuite
import day16.TicketTranslation._
import utils.InputUtils.read

class TicketTranslationTest extends BaseTestSuite {

  val example =
    """class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12""".stripMargin.stripMargin.split("\n").toList

  val example2 =
    """class: 0-1 or 4-19
      |row: 0-5 or 8-19
      |seat: 0-13 or 16-19
      |
      |your ticket:
      |11,12,13
      |
      |nearby tickets:
      |3,9,18
      |15,1,5
      |5,14,9""".stripMargin.stripMargin.split("\n").toList

  "Ticket Translation" should "parse rules" in {
    parseRules(example.take(3)) shouldBe List(
      Rule("class", Seq(1 to 3, 5 to 7)),
      Rule("row", Seq(6 to 11, 33 to 44)),
      Rule("seat", Seq(13 to 40, 45 to 50)),
    )
  }

  it should "compute the error rate (example)" in {
    val input = parseInput(example)
    errorRate(input.rules, input.nearbyTickets) shouldBe 71
  }

  it should "compute the error rate - part 1" in {
    val input = parseInput(read("day16.txt"))
    errorRate(input.rules, input.nearbyTickets) shouldBe 21071
  }

  it should "keep clean tickets" in {
    val input = parseInput(example)
    cleanTickets(input.rules, input.nearbyTickets) should have size 1

    val input2 = parseInput(example2)
    cleanTickets(input2.rules, input2.nearbyTickets) should have size 3
  }

  it should "identify columns (example)" in {
    val input = parseInput(example2)
    identifyColumns(input.rules,
      cleanTickets(input.rules, input.nearbyTickets))
  }

  it should "get product of departure values - part 2" in {
    val input = parseInput(read("day16.txt"))
    val cols = identifyColumns(input.rules,
      cleanTickets(input.rules, input.nearbyTickets))
    getValuesDepartures(input.myTicket, cols) shouldBe 3429967441937L
  }
}
