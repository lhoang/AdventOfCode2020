package day24

import scala.annotation.tailrec

object LobbyLayout {

  /**
   * Coordinates in Hexa :
   *      |-1,1 | 0,1 | 1,1
   * /-1,0 \ / 0,0 \ / 1,0 \
   *      |-1,-1|0,-1 | 1,-1
   */
  case class Coord(x:Int, y:Int) {
    def moveX(i:Int): Coord = this.copy(x = this.x + i)
    def moveY(j:Int): Coord = this.copy(y = this.y + j)
  }

  def splitDir(s: String) : Seq[String] = {
    @tailrec
    def parse(remaining: String, acc: List[String]): List[String] = {
      if (remaining.isEmpty) acc
      else {
        remaining.head match {
          case c if "ns".contains(c) => parse(remaining.drop(2), remaining.take(2) :: acc)
          case c => parse(remaining.tail, c.toString :: acc)
        }
      }
    }
    parse(s, List.empty[String]).reverse
  }

  def idTile(s:String, origin: Coord = Coord(0, 0)): Coord = {
    val directions = splitDir(s)
    directions.foldLeft(origin)((current, dir) => {
      dir match {
        case "e" => current.moveX(1)
        case "w" => current.moveX(-1)
        case "se" =>
          if (current.y % 2 == 0) current.moveY(-1) else current.moveY(-1).moveX(1)
        case "ne" =>
          if (current.y % 2 == 0) current.moveY(1) else current.moveY(1).moveX(1)
        case "sw" =>
          if (current.y % 2 == 0) current.moveY(-1).moveX(-1) else current.moveY(-1)
        case "nw" =>
          if (current.y % 2 == 0) current.moveY(1).moveX(-1) else current.moveY(1)
      }
    })
  }

  def flipTiles(input:List[String]) : Set[Coord] = {
    input.foldLeft(Set.empty[Coord])((blacks, line) => {
      val tile = idTile(line)
      if (blacks(tile)) blacks - tile
      else blacks + tile
    })
  }

  def countAdjacentBlackTiles(tile: Coord, blacks: Set[Coord]) : Int = {
    Seq("ne", "e", "se", "sw", "w", "nw")
      .map(idTile(_, tile))
      .count(blacks(_))
  }

  def playRound(blacks: Set[Coord]): Set[Coord] = {
    val x = blacks.map(_.x)
    val y = blacks.map(_.y)
    val xRange = x.min -1 to x.max +1
    val yRange =  y.min -1 to y.max +1

    val newTiles = for {
      i <- xRange
      j <- yRange
      tile = Coord(i,j)
      count = countAdjacentBlackTiles(tile, blacks)
    } yield {
      if (blacks(tile) && (count == 0 || count > 2)) (null, tile)
      else if (count == 2) (tile, null)
      else (null, null)
    }
    val (newBlacks, newWhites0) = newTiles.unzip
    val whites = newWhites0.filter(_ != null)
    blacks.filterNot(whites.contains(_)) ++ newBlacks.filter(_ != null)
  }

  def play(input:List[String], days: Int) : Int  = {
    val init = flipTiles(input)
    (1 to days).foldLeft(init)((blacks, _) => playRound(blacks)).size
  }
}
