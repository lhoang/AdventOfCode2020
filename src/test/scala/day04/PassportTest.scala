package day04

import day04.Passport._
import utils.BaseTestSuite
import org.scalatest.PartialFunctionValues._
import utils.InputUtils.read

class PassportTest extends BaseTestSuite {

  val example =
    """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
      |byr:1937 iyr:2017 cid:147 hgt:183cm
      |
      |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
      |hcl:#cfa07d byr:1929
      |
      |hcl:#ae17e1 iyr:2013
      |eyr:2024
      |ecl:brn pid:760753108 byr:1931
      |hgt:179cm
      |
      |hcl:#cfa07d eyr:2025 pid:166559648
      |iyr:2011 ecl:brn hgt:59in""".stripMargin.split("\n").toList


  val exampleValid =
    """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
      |hcl:#623a2f
      |
      |eyr:2029 ecl:blu cid:129 byr:1989
      |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
      |
      |hcl:#888785
      |hgt:164cm byr:2001 iyr:2015 cid:88
      |pid:545766238 ecl:hzl
      |eyr:2022
      |
      |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""".stripMargin.split("\n").toList

  val exampleInvalid =
    """eyr:1972 cid:100
      |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
      |
      |iyr:2019
      |hcl:#602927 eyr:1967 hgt:170cm
      |ecl:grn pid:012533040 byr:1946
      |
      |hcl:dab227 iyr:2012
      |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
      |
      |hgt:59cm ecl:zzz
      |eyr:2038 hcl:74454a iyr:2023
      |pid:3556412378 byr:2007""".stripMargin.split("\n").toList

  "Passport" should "parse inputs" in {
    val res = parseInput(example)
    res.size shouldBe 4
  }

  it should "parse Passport" in {
    val pass = parsePassport("hcl:#cfa07d eyr:2025 pid:166559648")
    pass should contain key "hcl"
    pass should contain key "eyr"
    pass should contain key "pid"
    pass.valueAt("hcl") shouldBe "#cfa07d"
  }

  it should "count the valid passports (example)" in {
    countValidPassports(example) shouldBe 2
  }

  it should "count the valid passports - Part 1" in {
    countValidPassports(read("day4.txt")) shouldBe 256
  }

  it should "validate byr" in {
    validByr("2002") shouldBe true
    validByr("2003") shouldBe false
  }

  it should "validate iyr" in {
    validIyr("2020") shouldBe true
    validIyr("2009") shouldBe false
  }

  it should "validate eyr" in {
    validEyr("2020") shouldBe true
    validEyr("2031") shouldBe false
  }

  it should "validate hgt" in {
    validHgt("174cm") shouldBe true
    validHgt("194cm") shouldBe false
    validHgt("144cm") shouldBe false
    validHgt("59in") shouldBe true
    validHgt("58in") shouldBe false
    validHgt("77in") shouldBe false
    validHgt("in") shouldBe false
    validHgt("166") shouldBe false
  }

  it should "validate Hcl" in {
    validHcl("#00ffaf") shouldBe true
    validHcl("#00ffaff") shouldBe false
    validHcl("#00ffaF") shouldBe false
    validHcl("00ffaFF") shouldBe false
    validHcl("#00ffa") shouldBe false
    validHcl("#00ffag") shouldBe false
  }

  it should "validate Ecl" in {
    validEcl("amb") shouldBe true
    validEcl("blue") shouldBe false
    validEcl("brO") shouldBe false
  }

  it should "validate Pid" in {
    validPid("000019189") shouldBe true
    validPid("1010910901") shouldBe false
    validPid("1090909s9") shouldBe false
  }

  it should "count strict valid passports (example)" in {
    countStrictValidPassports(exampleValid) shouldBe 4
  }

  it should "count strict invalid passports (example)" in {
    countStrictValidPassports(exampleInvalid) shouldBe 0
  }

  it should "count strict valid passports - Part 2" in {
    countStrictValidPassports(read("day4.txt")) shouldBe 198
  }

}
