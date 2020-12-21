package day20

import utils.InputUtils.splitInput

import scala.annotation.tailrec
import scala.math.sqrt

object Jigsaw {

  case class Transformation(id: Int, rotation: Int, flip: String)

  case class Tile(id: Int, content: List[String],
                        up: Option[Int] = None, down: Option[Int]= None,
                        left: Option[Int]= None, right: Option[Int]= None) {
    def countNeighbours(): Int =
      Seq(up, down, left, right).flatten.size

    /**
     * Rotate 90° Left
     */
    def rotate(): Tile = this.copy(
      content = this.content.transpose.map(_.mkString).reverse,
      up= this.right,
      left= this.up,
      down=this.left,
      right=this.down
    )

    /**
     * Flip vertically.
     */
    def flipV(): Tile = this.copy(
      content = this.content.reverse,
      up= this.down,
      down=this.up,
    )

    /**
     * Flip horizontally.
     */
    def flipH(): Tile = this.copy(
      content = this.content.map(_.reverse),
      left= this.right,
      right=this.left,
    )

    def leftBorder():String = this.content.transpose.head.mkString
    def rightBorder():String = this.content.transpose.last.mkString
    def upBorder():String = this.content.head
    def downBorder():String = this.content.last


//    def flip():PlacedTile = {
//
//    }
  }

  def parseTiles(input: List[String]): List[Tile] =
    splitInput(input)
      .map(lines => {
        Tile(id = lines.head.split(" ").last.dropRight(1).toInt,
          content = lines.tail)
      })

  def possibleBorders(tile: Tile) = {
    //    val Seq(up, down, left, right) = normal

    //    val r = (seq:Seq[String]) => seq.map(_.reverse)
    //
    //    val flipH = r(Seq(up, down)) ++ Seq(left, right)
    //    val flipV = r(Seq(left, right)) ++ Seq(up, down)
    //    val r90 = Seq(right, left) ++ r(Seq(up, down))
    //    val r180 = r(Seq(down, up, right, left))
    //    val r270 = r(Seq(right, left)) ++ Seq(up, down)
    //    val r90H = r(Seq(right, left, down, up))
    //    val r270H = Seq(left, right, up, down)
    //
    //    normal.map(_ -> Transformation(tile.id, rotation = 0, flip = "")) ++
    //    flipH.map(_ -> Transformation(tile.id, rotation = 0, flip = "H")) ++
    //    flipV.map(_ -> Transformation(tile.id, rotation = 0, flip = "V")) ++
    //    r90.map(_ -> Transformation(tile.id, rotation = 90, flip = "")) ++
    //    r180.map(_ -> Transformation(tile.id, rotation = 180, flip = "")) ++
    //    r270.map(_ -> Transformation(tile.id, rotation = 270, flip = "")) ++
    //    r90H.map(_ -> Transformation(tile.id, rotation = 90, flip = "H")) ++
    //    r270H.map(_ -> Transformation(tile.id, rotation = 270, flip = "H"))

    val normal = borders(tile)
    normal ++ normal.map(_.reverse)
  }

  def borders(tile: Tile): Seq[String] = {
    val content = tile.content
    val cols = content.transpose.map(_.mkString)
    Seq(content.head, content.last, cols.head, cols.last)
  }

  def stats(tiles: List[Tile]) = {
    val allborders = tiles.flatMap(possibleBorders)
    val cornerBorders = allborders.groupMapReduce(identity)(_ => 1)(_ + _)
    cornerBorders
  }

  def generatePlacedTiles(tiles: List[Tile]): List[Tile] = {
    val tMap = tiles.flatMap(tile => {
      possibleBorders(tile).map(_ -> tile.id)
    }).groupBy(_._1)
      .view.mapValues(_.map(_._2))
      .toMap
    tiles.map(tile => {
      val Seq(up, down, left, right) =
        borders(tile).map(tMap.get(_).flatMap(_.find(_ != tile.id)))
      Tile(tile.id, tile.content, up, down, left, right)
    })
  }

  def findCorners(tiles: List[Tile]): List[Int] = {
    tiles.filter(_.countNeighbours() == 2)
      .map(_.id)
  }

  def rotateFlipUntil(tile: Tile, border: String,
                      method : Tile => String = (t:Tile)=>t.rightBorder(),
                      flip: Tile => Tile = (t:Tile)=>t.flipV()

                     ): Tile = {

    @tailrec
    def rec(current:Tile) : Tile = {
      if (Set(border, border.reverse)(method(current))) current
      else rec(current.rotate())
    }
    val rotated = rec(tile)
    if (method(rotated) == border) rotated
    else flip(rotated)

  }

  def placeTiles(raw: List[Tile]): List[List[Tile]] = {
    val tiles = generatePlacedTiles(raw)
    val tMap = tiles.map(tile => tile.id -> tile).toMap

    val size = sqrt(tiles.size).toInt

    val cornerIds = findCorners(tiles).toSet
    val corners = tiles.filter(v => cornerIds(v.id))

    //val ul = corners.find(tile => tile.up.isEmpty && tile.left.isEmpty).get
    val ur = corners.find(tile => tile.up.isEmpty && tile.right.isEmpty).get
   // val rl = corners.find(tile => tile.down.isEmpty && tile.left.isEmpty).get
    //val dr = corners.find(tile => tile.down.isEmpty && tile.right.isEmpty).get

    def build(currentLine: List[Tile], image: List[List[Tile]]): List[List[Tile]] = {
      val current = currentLine.head
      val border = current.leftBorder()
      current.left match {
        case Some(id) =>
          val newTile = tMap(id)
          val found = rotateFlipUntil(newTile, border)
          build(found::currentLine, image)
        case None =>
          val last = currentLine.last
          last.down match {
            case Some(id) =>
              val newTile = tMap(id)
              val found = rotateFlipUntil(newTile, last.downBorder(),
                method = (t:Tile)=> t.upBorder(),
                flip= (t:Tile)=>t.flipH())
              build(List(found), currentLine :: image)
            case None => currentLine :: image
          }
      }
    }
    val res = build(List(ur), List.empty[List[Tile]] )

    res
  }


  def buildImage(tiles: List[List[Tile]]): List[String] = {
    tiles.map(
      _.map(_.content)
//        .map(_.drop(1).dropRight(1)
//              .transpose
//              .drop(1).dropRight(1)
//              .transpose)
        .map(_.map(_.mkString)
              .mkString(" | "))
        .mkString("\n")
    )
  }

}
