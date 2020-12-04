package day04

import scala.util.Try

object Passport {

  val mandatoryFields = Set(
    "byr", "iyr", "eyr", "hgt",
    "hcl", "ecl", "pid"
  )

  def parseInput(input: List[String]): List[Map[String, String]] =
    input.map(line => if (line.isEmpty) "@@@" else line)
      .mkString(" ")
      .split("@@@")
      .toList
      .map(_.trim)
      .map(parsePassport)

  def parsePassport(line: String): Map[String, String] =
    line.split(" ")
      .map(entry => {
        val Array(key, value) = entry.split(":")
        key -> value
      })
      .toMap

  def validatePassport(pass: Map[String, String]): Boolean =
    mandatoryFields.forall(pass.contains)

  def countValidPassports(input: List[String]): Int =
    parseInput(input).count(validatePassport)


  def strictValidatePassport(pass: Map[String, String]): Boolean = {
    mandatoryFields.forall(pass.contains) &&
      pass.forall{
        case ("byr", value) => validByr(value)
        case ("iyr", value) => validIyr(value)
        case ("eyr", value) => validEyr(value)
        case ("hgt", value) => validHgt(value)
        case ("hcl", value) => validHcl(value)
        case ("ecl", value) => validEcl(value)
        case ("pid", value) => validPid(value)
        case _ => true
      }
  }

  def countStrictValidPassports(input: List[String]): Int =
    parseInput(input).count(strictValidatePassport)


  private def intIn(value: String, start: Int, end: Int): Boolean = {
    val year = Try(value.toInt).getOrElse(0)
    start <= year && year <= end
  }

  def validByr(value: String): Boolean = intIn(value, 1920, 2002)

  def validIyr(value: String): Boolean = intIn(value, 2010, 2020)

  def validEyr(value: String): Boolean = intIn(value, 2020, 2030)

  def validHgt(value: String): Boolean = {
    val regex = """(\d+)(\w{2})""".r
    value match {
      case regex(h, unit) if unit == "cm" => intIn(h, 150, 193)
      case regex(h, unit) if unit == "in" => intIn(h, 59, 76)
      case _ => false
    }
  }

  def validHcl(value: String): Boolean = """#[0-9a-f]{6}$""".r.matches(value)

  def validEcl(value: String): Boolean = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")(value)

  def validPid(value: String): Boolean = """^\d{9}$""".r.matches(value)
}
