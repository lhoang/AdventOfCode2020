package day18

import scala.annotation.tailrec
import scala.util.matching.Regex

object OperationOrder {

  val simple: String => Long = split _ andThen evalOp
  val advanced: String => Long = evalOpAdvanced

  def eval(s: String, method: String => Long = simple): Long = {
    @tailrec
    def recEvalParenthesis(s: String): String =
      if (s.contains('('))
        recEvalParenthesis(evalParenthesis(s, method))
      else s

    val simple = recEvalParenthesis(s)
    method(simple)
  }

  def split(s: String): List[String] =
    s.split(" ").toList

  @tailrec
  def evalOp(s: List[String]): Long =
    s match {
      case a :: Nil => a.toLong
      case a :: op :: b :: tail =>
        val head = (op match {
          case "+" => a.toLong + b.toLong
          case "*" => a.toLong * b.toLong
        }).toString
        evalOp(head :: tail)
    }

  def evalParenthesis(s: String, method: String => Long = simple): String = {
    val inside = new Regex("""\(([\d\+\* ]+)\)""", "content")
    inside.replaceAllIn(s, matcher => {
      method(matcher.group("content")).toString
    })
  }

  def evalAllAndSum(input: List[String], method: String => Long = simple): Long =
    input.map(eval(_, method)).sum

  def evalAddFirst(s: String): String = {
    val mult = new Regex("""\d+ ( ?\+ \d+)+""")
    mult.replaceAllIn(s,
      _.group(0)
        .split(" ")
        .filterNot(_ == "+")
        .map(_.toLong)
        .sum
        .toString
    )
  }

  def evalOpAdvanced(s: String): Long =
    evalAddFirst(s)
      .split(" ")
      .filterNot(_ == "*")
      .map(_.toLong)
      .product
}
