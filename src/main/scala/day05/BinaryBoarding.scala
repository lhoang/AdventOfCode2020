package day05

import scala.annotation.tailrec

object BinaryBoarding {

  case class Seat(row: Int, column: Int, id: Int)

  val row = "FB"
  val column = "LR"

  @tailrec
  def bisect(remaining: String, start: Int, end: Int, typeRC: String): Int =
    remaining.toList match {
      case c :: Nil => if (c == typeRC.head) start else end
      case c :: tail =>
        if (c == typeRC.head)
          bisect(tail.mkString, start, (start + end) / 2, typeRC)
        else bisect(tail.mkString, (start + end) / 2 + 1, end, typeRC)
    }

  def findSeat(pass: String): Seat = {
    val r = bisect(pass.take(7), 0, 127, row)
    val c = bisect(pass.takeRight(3), 0, 7, column)
    val id = r * 8 + c
    Seat(row = r, column = c, id = id)
  }

  def findHighestSeatId(input: List[String]): Int =
    input.map(findSeat)
      .map(_.id)
      .max

  def findMySeat(input: List[String]): Int = {
    input.map(findSeat)
      .map(_.id)
      .sorted
      .sliding(2)
      .find { case Seq(a, b) => a == b - 2 }
      .map(_.head + 1)
      .getOrElse(0)
  }
}
