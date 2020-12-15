package day15

import scala.collection.mutable.{Map => MMap}

object RambunctiousRecitation {

  /**
   * No Optimization : Mutable Map, prepend
   * 2020 : 193ms
   */
  def generate(init: List[Int], n: Int): List[Int] = {
    val spoken = MMap.from(
      init.zipWithIndex
        .map { case (x, i) => (x, List(i)) }
    )
    (init.size until n).foldLeft(init)((acc, i) => {
      val v = spoken.get(acc.last)
        .filter(_.size > 1)
        .map { case a :: b :: _ => a - b }
        .getOrElse(0)
      spoken(v) = i :: spoken.get(v).map(a => List(a.head)).getOrElse(Nil)
      acc :+ v
    })
  }

  /**
   * First Optimization : pas de gestion de list
   * 2020 : 86ms
   * 30_000_000 : 13s998ms
   */
  def generate2(init: List[Int], n: Int): Int = {
    val spoken = MMap.from(
      init.zipWithIndex
        .map { case (x, i) => (x, List(i)) }
    )
    (init.size until n).foldLeft(init.last)((last, i) => {
      val v = spoken.get(last)
        .filter(_.size > 1)
        .map { case a :: b :: _ => a - b }
        .getOrElse(0)
      spoken(v) = i :: spoken.get(v).map(a => List(a.head)).getOrElse(Nil)
      v
    })
  }

  /**
   * Optimized version :
   * mutable Array, direct lookup
   *
   *  2020 : 22ms
   *  30_000_000 : 4s32ms
   */
  def generate3(init: List[Int], n: Int): Int = {
    var spoken = new Array[Array[Int]](n)
    init.zipWithIndex
      .foreach { case (x, i) => spoken(x) = Array(i) }

    val res = (init.size until n).foldLeft(init.last)((last, i) => {
      val record = spoken(last)
      val v = if (record.length > 1) record(0) - record(1) else 0
      val existing = spoken(v)
      if (existing == null) spoken(v) = Array(i)
      else spoken(v) = Array(i, existing.head)
      v
    })
    spoken = null
    System.gc()
    res
  }
}
