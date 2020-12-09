package day09

import scala.annotation.tailrec

object Xmas {

  def parse(input: List[String]): List[Long] = input.map(_.toLong)

  def checkPreamble(n:Long, preamble: List[Long]): Boolean =
    preamble.combinations(2).exists(_.sum == n)

  def findWeakness(data: List[Long], preambleSize: Int): Long =
    data.sliding(preambleSize+1)
      .map( s => (s.last, s.take(preambleSize)) )
      .find{case (n, preamble) => !checkPreamble(n, preamble)}
      .map(_._1)
      .getOrElse(0)

  def addItems(n: Long, index: Int, data: List[Long]): List[Long] = {
    @tailrec
    def rec(i: Int, acc: List[Long]): List[Long] = {
      if (i >= data.size || acc.sum >= n) acc
      else rec(i+1, data(i)::acc)
    }
    rec(index, List())
  }

  def computeWeakness(input: List[String], preambleSize: Int ): Long = {
    val data = parse(input)
    val n = findWeakness(data, preambleSize)
    data.indices.map(i => addItems(n, i, data))
      .find(_.sum == n)
      .map(s => s.min + s.max)
      .getOrElse(0)
  }

}
