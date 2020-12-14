package day14

import utils.BaseTestSuite
import day14.DockingData._
import utils.InputUtils.read

class DockingDataTest extends BaseTestSuite {

  val example =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0""".stripMargin.split("\n").toList

  val example2 =
    """mask = 000000000000000000000000000000X1001X
      |mem[42] = 100
      |mask = 00000000000000000000000000000000X0XX
      |mem[26] = 1""".stripMargin.split("\n").toList

  "Docking Data" should "parse lines" in {
    val input =
      """mask = 100X000X100X00XX1010X0001X11XX100110
        |mem[1] = 43619
        |mem[17642] = 12960
        |mem[54949] = 1594
        |mem[25705] = 17992
        |mem[28651] = 47662
        |mask = 1000X01110X1101X111X010XX110000X0010
        |mem[12764] = 8190
        |mem[19395] = 450940
        |mem[29461] = 13796
        |mem[7984] = 2208
        |mem[1] = 224803""".stripMargin.split("\n").toList

    val state = readLinesV1(input)
    state.mask shouldBe "1000X01110X1101X111X010XX110000X0010"
    state.registry should have size 9
  }

  it should "write 36-bit binary String" in {
    to36bin(73) shouldBe "000000000000000000000000000001001001"
  }

  it should "apply mask" in {
    val res = applyMask(11, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    res shouldBe BigInt(73)
  }

  it should "compute the sum of the registry values (example)" in {
    computeSumValues(example, version = 1) shouldBe 165
  }

  it should "compute the sum of the registry values - part 1" in {
    computeSumValues(read("day14.txt"), version = 1) shouldBe BigInt("11179633149677")
  }

  it should "generate all addresses" in {
    generateAllAddresses("0001X0XX") should contain allElementsOf List(
      16, 17, 18, 19, 24, 25, 26, 27).map(c => BigInt(c))
  }

  it should "apply mask adress" in {
    val res = applyMaskAddress(26, "00000000000000000000000000000000X0XX")
    res should contain allElementsOf List(
      16, 17, 18, 19, 24, 25, 26, 27).map(c => BigInt(c))
  }

  it should "compute the sum of the registry values v2 (example)" in {
    computeSumValues(example2, version = 2) shouldBe 208
  }

  it should "compute the sum of the registry values v2 - part 2" in {
    computeSumValues(read("day14.txt"), version = 2) shouldBe BigInt("4822600194774")
  }
}
