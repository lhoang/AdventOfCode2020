package day20

import utils.BaseTestSuite
import day20.Jigsaw._
import utils.InputUtils.read

class JigsawTest extends BaseTestSuite {

  "Jigsaw" should "parse tiles" in {
    val res = parseTiles(read("day20_example.txt"))
    res should have size 9
    res.head.id shouldBe 2311
  }

  it should "parse tiles (part1)" in {
    val res = parseTiles(read("day20.txt"))
    res should have size 144
    //stats(res)
  }

  it should "count neighbours" in {
    Tile(1, Nil, up=Some(2)).countNeighbours() shouldBe 1
    Tile(1, Nil, Some(2),Some(3), Some(4), Some(5)).countNeighbours() shouldBe 4
  }

  it should "rotate tile" in {
    val tile = Tile(1, List("abc", "def", "ghi"),
      Some(2),Some(3), Some(4), Some(5))

    tile.rotate() shouldBe Tile(1, List("cfi", "beh", "adg"),
      Some(5),Some(4), Some(2), Some(3))
  }

  it should "flip tile" in {
    val tile = Tile(1, List("abc", "def", "ghi"),
      Some(2),Some(3), Some(4), Some(5))

    tile.flipV() shouldBe Tile(1, List("ghi", "def", "abc"),
      Some(3),Some(2), Some(4), Some(5))
  }

  it should "rotate until found" in {
    val tile = Tile(1, List("abc", "def", "ghi"),
      Some(2),Some(3), Some(4), Some(5))

    rotateFlipUntil(tile, "gda") shouldBe
      Tile(1, List("ihg", "fed", "cba"),
      Some(3),Some(2), Some(5), Some(4))

    rotateFlipUntil(tile, "adg") shouldBe
      Tile(1, List("cba", "fed", "ihg"),
        Some(2),Some(3), Some(5), Some(4))
  }


  it should "find neighbours for each tile" in {
    val tiles = parseTiles(read("day20_example.txt"))
    val res = generatePlacedTiles(tiles)
    res should have size 9
  }


  it should "find corners (example)" in {
    val tiles = parseTiles(read("day20_example.txt"))
    val res = findCorners(generatePlacedTiles(tiles))
      .map(_.toLong)
      .product
    res shouldBe 20899048083289L
  }

  it should "find corners - part 1" in {
    val tiles = parseTiles(read("day20.txt"))
    val res = findCorners(generatePlacedTiles(tiles))
      .map(_.toLong)
      .product
    res shouldBe 29584525501199L
  }

  it should "place tiles (example)" in {
    val tiles = parseTiles(read("day20_example.txt"))
    placeTiles(tiles)
  }

  it should "place tiles" in {
    val tiles = parseTiles(read("day20.txt"))
    val res = placeTiles(tiles)

    res.map(_.map(_.id).mkString(", "))
      .foreach(println)
  }

  it should "build image (example)" in {
    val tiles = parseTiles(read("day20_example.txt"))
    val res = placeTiles(tiles)
    res.map(_.map(_.id).mkString(", "))
      .foreach(println)
    val image = buildImage(res)

    image.foreach(println)
  }
}
