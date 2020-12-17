package day17

import scala.math._

object ConwayCubes {

  type Cube = List[List[String]]

  def countNeighbours(x: Int, y: Int, z: Int, cube: Cube): Int = {
    val height = cube.length - 1
    val neighbours = for {
      k <- max(z - 1, 0) to min(z + 1, height)
      j <- max(y - 1, 0) to min(y + 1, height)
      i <- max(x - 1, 0) to min(x + 1, height)
      if i != x || j != y || k != z
      if cube(k)(j)(i) == '#'
    } yield (i, j, k)
    //println(neighbours.mkString(", "))
    neighbours.size
  }

  def from2dTo3d(flat: List[String]): Cube = {
    val size = flat.size
    val newZ = List.fill(size)("." * size)
    List.fill((size/2.0).ceil.toInt-1)(newZ) ++ (flat :: List.fill(size/2)(newZ))
    //List(newZ,flat,newZ)
  }

  def biggerCube(cube: Cube): Cube = {
    val size = cube.size + 2
    val newZ = List.fill(size)("." * size)
    val existing = cube.map(xy => ("." * size :: xy.map("." + _ + ".")) :+ "." * size)
    (newZ :: existing) :+ newZ
  }


  def playRound(prevCube: Cube, display: Boolean= false): Cube = {
    val cube = biggerCube(prevCube)

    val height = cube.length - 1
    val newCube = for {
      k <- 0 to height
      j <- 0 to height
      i <- 0 to height
    } yield {
      val occupied = countNeighbours(i, j, k, cube)
      cube(k)(j)(i) match {
        case '.' if occupied == 3 => '#'
        case '#' if Set(2, 3)(occupied) => '#'
        case _ => '.'
        //case c => c
      }
    }
        val t1 = newCube.grouped(pow(height+1,2).toInt).toList
        val res = t1.map(_.grouped(height+1).map(_.mkString).toList)

        if (display) displayCube(res)
        res
  }

  def displayCube(cube: Cube): Unit = {
    println
    cube.zipWithIndex
      .foreach{case(xy, z) =>
        println(s"z=$z")
        xy.foreach(println)
        println
      }
    println
    println
  }

  def countActives(cube: Cube): Int =
    cube.flatten.flatten.count(_ == '#')

  def runRounds(input:List[String], n: Int, display: Boolean = false): Cube = {
    val first = from2dTo3d(input)
    if (display) displayCube(first)
    (1 to n).foldLeft(first)((cube, _) => playRound(cube, display))
  }
}
