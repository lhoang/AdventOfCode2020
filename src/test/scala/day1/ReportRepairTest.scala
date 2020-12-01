package day1

import utils.BaseTestSuite
import utils.InputUtils.read

class ReportRepairTest extends BaseTestSuite{

  val sut = new ReportRepair

  "Report Repair" should "find 2 entries  == 2020 (example)" in {
    val inputs = List("1721", "979", "366", "299", "675", "1456")
    val res = sut.find2Entries(inputs)
    res shouldBe 514579
  }

  it should "give the answer to the challenge - Part 1" in {
    val res = sut.find2Entries(read("day1.txt"))
    res shouldBe 926464
  }


  it should "find 3 entries  == 2020 (example)" in {
    val inputs = List("1721", "979", "366", "299", "675", "1456")
    val res = sut.find3Entries(inputs)
    res shouldBe 241861950
  }

  it should "give the answer to the challenge - Part 2" in {
    val res = sut.find3Entries(read("day1.txt"))
    res shouldBe 65656536
  }

}
