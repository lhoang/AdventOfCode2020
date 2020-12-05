package day05

import day05.BinaryBoarding._
import utils.BaseTestSuite
import utils.InputUtils.read

class BinaryBoardingTest extends BaseTestSuite {

  "Binary Boarding" should "bisect" in {
    bisect("FBFBBFF", 0, 127, row) shouldBe 44
    bisect("BFFFBBF", 0, 127, row) shouldBe 70
    bisect("FFFBBBF", 0, 127, row) shouldBe 14
    bisect("RLR", 0, 7, column) shouldBe 5
    bisect("RLL", 0, 7, column) shouldBe 4
  }

  it should "find seat" in {
    findSeat("FBFBBFFRLR") shouldBe Seat(44, 5, 357)
    findSeat("BFFFBBFRRR") shouldBe Seat(70, 7, 567)
    findSeat("FFFBBBFRRR") shouldBe Seat(14, 7, 119)
    findSeat("BBFFBBFRLL") shouldBe Seat(102, 4, 820)
  }

  it should "find highest seat id - part 1" in {
    findHighestSeatId(read("day5.txt")) shouldBe 826
  }

  it should "find my seat - part 2" in {
    findMySeat(read("day5.txt")) shouldBe 678
  }
}
