package day02

import scala.util.matching.Regex

object Password {
  // Ex: 1-3 a: abcde
  val parser: Regex = """(\d+)-(\d+) ([a-z]): (.*)""".r

  def checkPassword(s: String): Boolean = s match {
    case parser(min, max, char, password) =>
      (min.toInt to max.toInt).contains(password.count(_ == char.head))
    case _ => false
  }

  def countValidPasswords(lines: List[String]): Int =
    lines.count(checkPassword)

  def checkPasswordWithPosition(s: String): Boolean = s match {
    case parser(min, max, char, password) =>
      Seq(min, max)
        .map(_.toInt - 1)
        .map(password)
        .count(_ == char.head) == 1
    case _ => false
  }

  def countValidPasswordsWithPosition(lines: List[String]): Int =
    lines.count(checkPasswordWithPosition)

}
