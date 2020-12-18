package day18

import utils.BaseTestSuite
import day18.OperationOrder._
import utils.InputUtils.read

class OperationOrderTest extends BaseTestSuite {

  "Operation Order" should "evaluate simple operation"  in {
    evalOp(split("1 + 2 * 3 + 4 * 5 + 6")) shouldBe 71
  }

  it should "eval parenthesis" in {
    evalParenthesis("2 * 3 + (4 * 5)") shouldBe "2 * 3 + 20"
  }

  it should "eval examples" in {
    eval("1 + (2 * 3) + (4 * (5 + 6))") shouldBe 51
    eval("5 + (8 * 3 + 9 + 3 * 4 * 3)") shouldBe 437
    eval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") shouldBe 12240
    eval("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") shouldBe 13632
  }

  it should "eval all and sum - part 1" in {
    evalAllAndSum(read("day18.txt")) shouldBe 4297397455886L
  }

  it should "eval addition first" in {
    evalAddFirst("1 + 2 * 3 + 4 * 5 + 6") shouldBe "3 * 7 * 11"
  }

  it should "eval advanced" in {
    evalOpAdvanced("1 + 2 * 3 + 4 * 5 + 6") shouldBe 231
    evalOpAdvanced("3 + 4 + 3") shouldBe 10
  }

  it should "eval examples, advanced" in {
    eval("1 + (2 * 3) + (4 * (5 + 6))", advanced) shouldBe 51
    eval("5 + (8 * 3 + 9 + 3 * 4 * 3)", advanced) shouldBe 1445
    eval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", advanced) shouldBe 669060
    eval("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", advanced) shouldBe 23340
  }


  it should "eval all and sum - part 2" in {
    evalAllAndSum(read("day18.txt"), advanced) shouldBe 93000656194428L
  }
}
