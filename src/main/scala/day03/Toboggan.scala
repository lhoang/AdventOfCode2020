package day03

import scala.annotation.tailrec

object Toboggan {

  def slide(mountain: List[String], right: Int, down: Int): String = {
    val width = mountain.head.length

    @tailrec
    def slideRec(x: Int, y: Int, path: String): String = {
      if (y >= mountain.size) path
      else slideRec((x + right) % width, y + down, path + mountain(y)(x))
    }

    slideRec(0, 0, "")
  }

  def countTrees(mountain: List[String], right: Int, down: Int): Long =
    slide(mountain, right, down).count(_ == '#')


  def countTreesSlopes(mountain: List[String], slopes: List[(Int, Int)]): Long =
    slopes.map { case (right, down) => countTrees(mountain, right, down) }
      .product

}
