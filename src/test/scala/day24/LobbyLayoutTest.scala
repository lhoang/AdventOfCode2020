package day24

import utils.BaseTestSuite
import day24.LobbyLayout._
import utils.InputUtils.read

class LobbyLayoutTest extends BaseTestSuite {

  val example =
    """sesenwnenenewseeswwswswwnenewsewsw
      |neeenesenwnwwswnenewnwwsewnenwseswesw
      |seswneswswsenwwnwse
      |nwnwneseeswswnenewneswwnewseswneseene
      |swweswneswnenwsewnwneneseenw
      |eesenwseswswnenwswnwnwsewwnwsene
      |sewnenenenesenwsewnenwwwse
      |wenwwweseeeweswwwnwwe
      |wsweesenenewnwwnwsenewsenwwsesesenwne
      |neeswseenwwswnwswswnw
      |nenwswwsewswnenenewsenwsenwnesesenew
      |enewnwewneswsewnwswenweswnenwsenwsw
      |sweneswneswneneenwnewenewwneswswnese
      |swwesenesewenwneswnwwneseswwne
      |enesenwswwswneneswsenwnewswseenwsese
      |wnwnesenesenenwwnenwsewesewsesesew
      |nenewswnwewswnenesenwnesewesw
      |eneswnwswnwsenenwnwnwwseeswneewsenese
      |neswnwewnwnwseenwseesewsenwsweewe
      |wseweeenwnesenwwwswnew
      |""".stripMargin.split("\n").toList

  "Lobby layout" should "parse directions" in {
    splitDir("esenee") shouldBe List("e", "se", "ne", "e")
    splitDir("nwwswee") shouldBe List("nw", "w", "sw", "e", "e")
  }

  it should "id tile" in {
    // from even y
    idTile("se") shouldBe Coord(0, -1)
    idTile("sw") shouldBe Coord(-1, -1)
    idTile("ne") shouldBe Coord(0, 1)
    idTile("nw") shouldBe Coord(-1, 1)

    // from  odd y
    idTile("nese") shouldBe Coord(1, 0)
    idTile("nesw") shouldBe Coord(0, 0)
    idTile("nene") shouldBe Coord(1, 2)
    idTile("nenw") shouldBe Coord(0, 2)

    idTile("se", Coord(0,1)) shouldBe Coord(1, 0)
    idTile("sw", Coord(0,1)) shouldBe Coord(0, 0)
    idTile("ne", Coord(0,1)) shouldBe Coord(1, 2)
    idTile("nw", Coord(0,1)) shouldBe Coord(0, 2)


    idTile("esenee") shouldBe Coord(3, 0)
    idTile("esew") shouldBe Coord(0, -1)
    idTile("nwwswee") shouldBe Coord(0, 0)
  }

  it should "count black tiles (example)" in {
    flipTiles(example).size shouldBe 10
  }

  it should "count black tiles - part 1" in {
    flipTiles(read("day24.txt")).size shouldBe 400
  }

  it should "count adjacent black tiles" in {
    val blacks = Set(
      Coord(2,2),
      Coord(0,0),
      Coord(1,0),
      Coord(-1,-1),
      Coord(0,-1)
    )
    countAdjacentBlackTiles(Coord(1,1), blacks) shouldBe 2
    countAdjacentBlackTiles(Coord(0,-1), blacks) shouldBe 3
    countAdjacentBlackTiles(Coord(-2,1), blacks) shouldBe 0
  }

  it should "play rounds (example)" in {
    val init = flipTiles(example)
    val day1 = playRound(init)
    day1.size shouldBe 15

    val day2 = playRound(day1)
    day2.size shouldBe 12

    val day3 = playRound(day2)
    day3.size shouldBe 25
  }

  it should "count black tiles after n days (example)" in {
    play(example, days = 2) shouldBe 12
    play(example, days = 5) shouldBe 23
    play(example, days = 10) shouldBe 37
    play(example, days = 100) shouldBe 2208
  }

  it should "count black tiles after n days - part 2" in {
    play(read("day24.txt"), days = 100) shouldBe 3768
  }
}
