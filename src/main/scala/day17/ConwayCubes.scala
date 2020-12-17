package day17

import scala.math._

object ConwayCubes {

  type Cube = List[List[String]]
  type HCube = List[List[List[String]]]

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



  def countHNeighbours(x: Int, y: Int, z: Int, w: Int, cube: HCube): Int = {
    val height = cube.length - 1
    val neighbours = for {
      l <- max(w - 1, 0) to min(w + 1, height)
      k <- max(z - 1, 0) to min(z + 1, height)
      j <- max(y - 1, 0) to min(y + 1, height)
      i <- max(x - 1, 0) to min(x + 1, height)
      if i != x || j != y || k != z || l != w
      if cube(l)(k)(j)(i) == '#'
    } yield (i, j, k, l)
    //println(neighbours.mkString(", "))
    neighbours.size
  }

  def biggerHCube(cube: HCube): HCube = {
    val size = cube.size + 2
    val newW = List.fill(size)(List.fill(size)("." * size))
    val existing = cube.map(xyz => biggerCube(xyz))
    (newW :: existing) :+ newW
  }

  def from2dTo4d(flat: List[String]): HCube = {
    val size = flat.size
    val newW = List.fill(size)(List.fill(size)("." * size))
    List.fill((size/2.0).ceil.toInt-1)(newW) ++ (from2dTo3d(flat) :: List.fill(size/2)(newW))
  }

  def playHRound(prevCube: HCube, display: Boolean= false): HCube = {
    val cube = biggerHCube(prevCube)

    val height = cube.length - 1
    val newCube = for {
      l <- 0 to height
      k <- 0 to height
      j <- 0 to height
      i <- 0 to height
    } yield {
      val occupied = countHNeighbours(i, j, k, l, cube)
      cube(l)(k)(j)(i) match {
        case '.' if occupied == 3 => '#'
        case '#' if Set(2, 3)(occupied) => '#'
        case _ => '.'
      }
    }
    val t1 = newCube.grouped(pow(height + 1, 3).toInt).toList
    val t2: List[List[IndexedSeq[Char]]] = t1.map(_.grouped(pow(height + 1, 2).toInt).toList)
    val res: List[List[List[String]]] = t2.map(_.map(_.grouped(height + 1).map(_.mkString).toList))

    if (display) displayHCube(res)
    res
  }

  def countHActives(cube: HCube): Int =
    cube.flatten.flatten.flatten.count(_ == '#')

  def runHRounds(input:List[String], n: Int, display: Boolean = false): HCube = {
    val first = from2dTo4d(input)
    if (display) displayHCube(first)
    (1 to n).foldLeft(first)((cube, _) => playHRound(cube, display))
  }

  def displayHCube(cube: HCube): Unit = {
    println
    cube.zipWithIndex
      .foreach{case(xyz, w) =>
        println(s"=== w=$w ==============")
        xyz.zipWithIndex.foreach{case(xy, z) =>
          println(s"== z=$z, w=$w ===")
          xy.foreach(println)
          println
        }
        println
      }
    println
    println
  }


}
