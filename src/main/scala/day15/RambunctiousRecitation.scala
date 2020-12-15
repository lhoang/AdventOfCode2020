package day15

import scala.collection.mutable.{Map => MMap}

object RambunctiousRecitation {

  def generate(init: List[Int], n: Int): List[Int] = {
    val spoken = MMap.from(
      init.zipWithIndex
        .map{case (x, i) => (x, List(i))}
    )
    (init.size until n).foldLeft(init)((acc, i) => {
      val v = spoken.get(acc.last)
        .filter(_.size > 1)
        .map{ case a::b::_ => a-b }
        .getOrElse(0)
      spoken(v) = i :: spoken.get(v).map(a => List(a.head)).getOrElse(Nil)
      acc :+ v
    })
  }

  // initial : 193ms
}
