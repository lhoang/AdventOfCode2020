package day02

import day02.Password._
import utils.BaseTestSuite
import utils.InputUtils.read

class PasswordTest extends BaseTestSuite {

  "Password" should "parse the rule and validate password" in {
    checkPassword("1-3 a: abcde") shouldBe true
    checkPassword("1-1 a: abcde") shouldBe true
    checkPassword("0-3 a: abcde") shouldBe true
    checkPassword("2-3 a: abcde") shouldBe false
    checkPassword("1-5 y: abcde") shouldBe false
    checkPassword("10-20 d: djddddccdbdddddddndd") shouldBe true
  }

  it should "count the valid passwords (example)" in {
    val rules = List(
      "1-3 a: abcde",
      "1-3 b: cdefg",
      "2-9 c: ccccccccc"
    )
    countValidPasswords(rules) shouldBe 2
  }

  it should "count the valid passwords - Part 1" in {
    countValidPasswords(read("day2.txt")) shouldBe 439
  }

  it should "parse validate password with position" in {
    checkPasswordWithPosition("1-3 a: abcde") shouldBe true
    checkPasswordWithPosition("2-9 c: ccccccccc") shouldBe false
    checkPasswordWithPosition("10-20 d: djddddccdbdddddddndd") shouldBe true
  }

  it should "count the valid passwords - Part 2" in {
    countValidPasswordsWithPosition(read("day2.txt")) shouldBe 584
  }

}
