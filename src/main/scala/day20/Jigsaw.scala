package day20

import utils.InputUtils.splitInput

import scala.annotation.tailrec

object Jigsaw {

  type Point = (Int,Int)
  type Image = List[String]

  case class Tile(id: Int, content: Image,
                  up: Option[Int] = None, down: Option[Int] = None,
                  left: Option[Int] = None, right: Option[Int] = None) {
    def countNeighbours(): Int =
      Seq(up, down, left, right).flatten.size

    /**
     * Rotate 90° Left
     */
    def rotate(): Tile = this.copy(
      content = this.content.transpose.map(_.mkString).reverse,
      up = this.right,
      left = this.up,
      down = this.left,
      right = this.down
    )

    /**
     * Flip vertically.
     */
    def flipV(): Tile = this.copy(
      content = this.content.reverse,
      up = this.down,
      down = this.up,
    )

    /**
     * Flip horizontally.
     */
    def flipH(): Tile = this.copy(
      content = this.content.map(_.reverse),
      left = this.right,
      right = this.left,
    )

    def leftBorder(): String = this.content.transpose.head.mkString

    def rightBorder(): String = this.content.transpose.last.mkString

    def upBorder(): String = this.content.head

    def downBorder(): String = this.content.last
  }

  def parseTiles(input: List[String]): List[Tile] =
    splitInput(input)
      .map(lines => {
        Tile(id = lines.head.split(" ").last.dropRight(1).toInt,
          content = lines.tail)
      })

  def possibleBorders(tile: Tile): Seq[String] = {
    val normal = borders(tile)
    normal ++ normal.map(_.reverse)
  }

  def borders(tile: Tile): Seq[String] = {
    val content = tile.content
    val cols = content.transpose.map(_.mkString)
    Seq(content.head, content.last, cols.head, cols.last)
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
                      method: Tile => String = (t: Tile) => t.rightBorder(),
                      flip: Tile => Tile = (t: Tile) => t.flipV()
                     ): Tile = {

    @tailrec
    def rec(current: Tile): Tile = {
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

    val cornerIds = findCorners(tiles).toSet
    val corners = tiles.filter(v => cornerIds(v.id))

    val ur = corners.find(tile => tile.up.isEmpty && tile.right.isEmpty).get

    def build(currentLine: List[Tile], image: List[List[Tile]]): List[List[Tile]] = {
      val current = currentLine.head
      val border = current.leftBorder()
      current.left match {
        case Some(id) =>
          val newTile = tMap(id)
          val found = rotateFlipUntil(newTile, border)
          build(found :: currentLine, image)
        case None =>
          val last = currentLine.last
          last.down match {
            case Some(id) =>
              val newTile = tMap(id)
              val found = rotateFlipUntil(newTile, last.downBorder(),
                method = (t: Tile) => t.upBorder(),
                flip = (t: Tile) => t.flipH())
              build(List(found), currentLine :: image)
            case None => currentLine :: image
          }
      }
    }

    val res = build(List(ur), List.empty[List[Tile]])

    res.reverse
  }


  def buildImage(allTiles: List[List[Tile]]): Image = {
    allTiles.flatMap(row =>
      row.map(tile => tile.content
        .drop(1).dropRight(1)
        .transpose
        .drop(1).dropRight(1)
        .transpose
        .map(_.mkString)
      )
        .transpose
        //.map(_.mkString(" | "))
        .map(_.mkString)
    )
  }

  def printImage(image: Image) =
    println(image
      //      .mkString("\n\n")
      .mkString("\n")
    )


  def findSeaMonsters(image: Image): (Tile, List[Point]) = {
    val tile = Tile(id = 0, content = image)
    val tWidth = image.head.length
    val tHeight = image.size

    val (patternCoords, pWidth, pHeight) = seaMonsterPattern()

    def count(content: Image): List[Point] = {
      (for {
        j <- 0 until (tHeight - pHeight)
        i <- 0 until (tWidth - pWidth)
        if patternCoords.forall(point => {
          val (x, y) = point
          content(j + y)(i + x) == '#'
        })
      } yield (i, j))
        .toList
    }

    def rotateFlipUntilSeaMonsterFound(tile: Tile):(Tile, List[Point]) = {

      val res = Seq(tile, tile.rotate(), tile.rotate().rotate(),
        tile.flipV(), tile.flipH(),
        tile.rotate().flipH(), tile.rotate().flipV())
        .map(t => (t, count(t.content)))

      res.find(_._2.nonEmpty)
        .getOrElse((tile, List.empty[Point]))
    }

    rotateFlipUntilSeaMonsterFound(tile)
  }


  def seaMonsterPattern(): (List[Point], Int, Int) = {
    val pattern =
      """..................#.
        |#....##....##....###
        |.#..#..#..#..#..#...""".stripMargin.split("\n")
    val pWidth = pattern.head.length
    val pHeight = pattern.size

    val coords = (for {
      j <- pattern.indices
      i <- 0 until pWidth
      if pattern(j)(i) == '#'
    } yield (i, j)).toList
    (coords, pWidth, pHeight)
  }

  def identifySeaMonsters(tile: Tile, found: List[Point]): Image = {
    val (patternCoords, _, _) = seaMonsterPattern()
    val tWidth = tile.content.head.length

    def highlight(tile: Tile, found: List[Point]): Image = {
      found.foldLeft(tile.content)((newTile, start)=> {
        val (offsetX, offsetY)= start
        val newPattern = patternCoords.map(point => {
          val (x, y) = point
          (x+offsetX, y+offsetY)
        })
        val res = for{
          j <- newTile.indices
          i <- newTile.head.indices
        } yield {
          if (newPattern.contains((i,j))) 'O'
          else newTile(j)(i)
        }
        res.grouped(tWidth)
          .map(_.mkString)
          .toList
      })
    }
    highlight(tile, found)
  }

  def waterRoughness(image: Image): Int = {
    image.flatten.count(_ == '#')
  }

}
