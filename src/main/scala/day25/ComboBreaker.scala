package day25

import scala.annotation.tailrec

object ComboBreaker {

  def div(n:Long, d: Long) : (Long, Long) = (n/d, n%d)

  val seed = 20201227L

  def generatePublicKey(subject: Long, loopSize: Int):Long = {
    def loop(value: Long): Long = {
      (value * subject) % seed
    }
    (1 to loopSize).foldLeft(1L)((acc, _) => loop(acc))
  }

  def findLoopSize(subject: Int, publicKey: Long): Int = {
    @tailrec
    def count(step: Int, res: Long) : Int = {
      if (res == publicKey) step
      else count(step+1, res* subject % seed)
    }
    count(0, 1L)
  }

  def findEncryptionKey(input: Seq[Long], subject: Int) : Long = {
    val Seq(loop1, loop2) = input.map(findLoopSize(subject,_))
    generatePublicKey(input(0), loop2)
  }
}
