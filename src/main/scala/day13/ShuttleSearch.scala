package day13

import scala.math.BigInt

object ShuttleSearch {

  def findNextShuttle(input: List[String]): (Int, Int) = {
    val ts = input.head.toInt
    val shuttles = input.last.split(",")
      .filter(_ != "x")
      .map(_.toInt)

    shuttles.map(n => (n - ts % n) % n -> n)
      //.tapEach(println)
      .min
  }

  def getAnswer(shuttle: (Int, Int)): Int = {
    shuttle._1 * shuttle._2
  }


  def validTs(shuttles: Array[(Int, Int)]): BigInt => Boolean =
    (ts: BigInt) => {
      shuttles.map { case (bus, i) =>
        (bus, i, ts % bus == (bus - i) % bus)
      }.forall(_._3)
  }

  def getShuttlesWithIndex(seq: String): Array[(Int, Int)] = {
    seq.split(",")
      .zipWithIndex
      .filter { case (bus, _) => bus != "x" }
      .map { case (bus, i) => bus.toInt -> i }
  }


  /**
   * Brut Force only work for the examples.
   *
   * @return
   */
  def findValidTsBrutForce(shuttles: Array[(Int, Int)]): BigInt = {
    val (max, index) = shuttles.max
    val isValidTs = validTs(shuttles)

    LazyList.iterate(BigInt(max - index))(_ + BigInt(max))
      .find(i => isValidTs(i))
      .getOrElse(BigInt(0))
  }


  /**
   * Find the multiple of x, n such that  n % div = 1
   */
  def findMultipleMod1(x: BigInt, div: Int): BigInt = {
    LazyList.iterate(x)(_ + x)
      .find(m => m % div == 1)
      .getOrElse(BigInt(0))
  }

  /**
   * Find the valid timestamp with the Chinese Remainder theorem.
   * https://en.wikipedia.org/wiki/Chinese_remainder_theorem.
   *
   * Warning: the departure shift is not exactly the remainder
   */
  def findValidTs(shuttles: Array[(Int, Int)]): BigInt = {
    val n = shuttles.map(i => BigInt(i._1)).product

    val sum = shuttles.map { case (div, departureShift) =>
      val m = findMultipleMod1(n / div, div)
      BigInt((div - departureShift) % div) * m
    }.sum

    sum % n
  }
}
