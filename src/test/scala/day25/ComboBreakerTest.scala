package day25

import utils.BaseTestSuite
import day25.ComboBreaker._

class ComboBreakerTest extends BaseTestSuite {

  val input = Seq(16616892L, 14505727L)
  val example = Seq(5764801L, 17807724L)

  "Combo Breaker" should "generate public key" in {
    generatePublicKey(subject = 7, loopSize = 8) shouldBe example(0)
    generatePublicKey(subject = 7, loopSize = 11) shouldBe example(1)
  }

  it should "find loopSize" in {
    findLoopSize(7, example(0)) shouldBe 8
    findLoopSize(7, example(1)) shouldBe 11
  }

  it should "find encryption key (example)" in {
    findEncryptionKey(example, 7) shouldBe 14897079L
  }

  it should "find encryption key - part 1" in {
    findEncryptionKey(input, 7) shouldBe 4441893L
  }
}
