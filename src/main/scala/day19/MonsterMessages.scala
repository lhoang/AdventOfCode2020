package day19

import utils.InputUtils.splitInput

import collection.mutable.{Map => MMap}
import scala.util.Try

object MonsterMessages {

  /**
   * Part 1 : Recursive approach as we know there is no loop
   * Start from 0
   */
  def parseRules(lines: List[String]): String = {
    val simpleCharR = """(\d+): "([a-z])"""".r
    val refsR = """(\d+): ([\d |]+)""".r

    val regexMap = MMap.empty[Int, String]

    val refs = lines.foldLeft(List.empty[(Int, String)])((acc, line) =>
      line match {
        case simpleCharR(index, value) =>
          regexMap(index.toInt) = value
          acc
        case refsR(index, refs) =>
          (index.toInt, refs) :: acc
        case c => println(c); acc
      }
    ).toMap

    def resolveRefs(index: Int): String = {
      regexMap.getOrElse(index, {
        val newExpr = buildExpr(refs(index))
        regexMap(index) = newExpr
        newExpr
      })
    }

    def buildExpr(s: String): String = {
      if (s.contains('|')) {
        s.split(" \\| ")
          .map(buildExpr)
          .mkString("(", "|", ")")
      } else {
        s.split(" ").map(_.toInt)
          .map(i => resolveRefs(i))
          .mkString("")
      }
    }

    resolveRefs(0)
  }

  /**
   * Part 2: There are cyclic dependencies with
   * 8: 42 | 42 8
   * 11: 42 31 | 42 11 31
   *
   * New approach : replace values until nothing can be done
   * End with 8, 11 and 0
   */
  def parseRulesFixed(lines: List[String]): String = {
    val simpleCharR = """(\d+): "([a-z])"""".r
    val refsR = """(\d+): ([\d ]+)""".r
    val refsOrR = """(\d+): ([\d |]+)""".r

    val regexMap = MMap.empty[Int, String]

    val refs = lines.foldLeft(List.empty[(Int, String)])((acc, line) =>
      line match {
        case simpleCharR(index, value) =>
          regexMap(index.toInt) = value
          acc
        case refsR(index, refs) =>
          (index.toInt, refs) :: acc
        case refsOrR(index, refs) =>
          (index.toInt, s"( $refs )") :: acc
        case c => println(c); acc
      }
    ).toMap

    def replace(current: Map[Int, String]): Unit = {
      val newMap = current.map(e => {
        val (key, value) = e
        val newValue = value.split(" ")
          .map(i => {
            Try(Integer.parseInt(i))
              .toOption
              .flatMap(v => regexMap.get(v))
              .getOrElse(i)
          }).mkString(" ")
        key -> newValue
      })
      val (remaining, found) = newMap.partition(_._2.exists(_.isDigit))
      found.foreach(e => regexMap(e._1) = e._2)
      if (found.nonEmpty) replace(remaining)
    }

    replace(refs)

    // Hardcoded...
    val Seq(p31, p42) = Seq(31, 42).map(key => regexMap(key))
      .map(_.replaceAll(" ", ""))

    // 8: 42 | 42 8
    val p8 = s"($p42){1,}"

    // 11: 42 31 | 42 11 31
    val p11 = (1 to 4).map(i => "(" + p42 * i + p31 * i + ")")
      .mkString("(", "|", ")")

    // 0: 8 11
    p8 + p11
  }


  def countValidMessages(input: List[String]): Int = {
    val List(rules, messages) = splitInput(input)
    val rule = parseRules(rules).r
    messages.count(rule.matches(_))
  }

  def countValidMessagesFixed(input: List[String]): Int = {
    val List(rules, messages) = splitInput(input)
    val rule = parseRulesFixed(rules).r
    messages.count(rule.matches(_))
  }

}
