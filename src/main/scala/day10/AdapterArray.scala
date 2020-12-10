package day10

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.math.pow

object AdapterArray {

  def parse(input: List[String]): List[Int] = input.map(_.toInt)

  def computeDiff(adapters: List[Int]): Map[Int, Int] = {
    val res = (adapters ++ List(0, adapters.max + 3))
      .sorted
      .sliding(2)
      .map(s => s(1) - s(0))
      .toSeq

    println(res.mkString(", "))
    res.groupMapReduce(identity)(_ => 1)(_ + _)
  }

  /**
   * Split when 2 numbers are not consecutives.
   */
  def splitInput(input: List[Int]): List[List[Int]] = {
    @tailrec
    def rec(remaining: List[Int], result: List[List[Int]]): List[List[Int]] = {
      remaining match {
        case last :: Nil => (last :: result.head) :: result.tail
        case a :: b :: tail if b - a > 1 => rec(b :: tail, List() :: ((a :: result.head) :: result.tail))
        case a :: b :: tail => rec(b :: tail, (a :: result.head) :: result.tail)
        case _ => result
      }
    }

    val split = rec(input.sorted, List(List()))
    split.map(_.reverse).reverse
  }

  def countCombinations(adapters: List[Int]): Long =
    splitInput(adapters ++ List(0, adapters.max + 3))
      .map(_.size)
      .map {
        // Tribonacci :
        // 3 consecutives numbers give 2 valid combos
        // 4 consecutives numbers give 4 valid combos
        case 1 => 1
        case 2 => 1
        case 3 => 2
        case 4 => 4
        case 5 => 7
        case 6 => 13
      }
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .map { case (key, exp) => pow(key, exp) }
      .product.toLong


  def countCombinationsWithPath(adapters: List[Int]): Long = {
    val acc:  MMap[Int, Long]= MMap()

    def rec(remaining: List[Int]): MMap[Int, Long] =
      remaining match {
        case Nil => acc
        case head :: tail => {
          tail.take(3)
            .filter(_ - head <= 3)
            .foreach(c => acc(c) = acc.getOrElse(c, 0L) + acc.getOrElse(head, 1L))
          rec(tail)
        }
      }

    val res = rec((adapters ++ List(0, adapters.max + 3)).sorted)
    res.last._2
  }

}
