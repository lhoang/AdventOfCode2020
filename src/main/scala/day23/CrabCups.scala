package day23

import scala.annotation.tailrec

object CrabCups {

  class Cup(val value: Int, var next: Cup = null)

  def findDestinationCup(currentValue: Int, excluded: Seq[Int], max: Int = 9): Int = {
    @tailrec
    def recFind(value: Int): Int = {
      val v = if (value == 1) max else value - 1
      if (!excluded.contains(v)) v
      else recFind(v)
    }

    recFind(currentValue)
  }

  /**
   * Naive implementation.
   */
  def play(input: String, n: Int): String = {
    (1 to n).foldLeft(input)((round, _) => playRound(round))
  }

  def playRound(input: String): String = {
    val cups = input.map(_.asDigit).toList
    val current :: tail = cups
    val (following3, remaining) = tail.splitAt(3)

    val destination = findDestinationCup(current, following3)
    val destinationIndex = remaining.indexOf(destination)
    val res = remaining.patch(destinationIndex + 1, following3, 0) :+ current
    res.mkString
  }

  def getFrom1(input: String): String = {
    input.split("1").reverse.mkString
  }



  /**
   * 2nd Implementation, optimized with LinkedList Cup 
   * and lookup array (index as Cup value)
   * 3.720ms
   * @return (current cup, cup 1)
   */
  def realPlay(input: String, nCups: Int, rounds: Int): (Cup, Cup) = {
    val init = input.map(_.asDigit)

    // Lookup array : contains ref to Cups
    // no 0, index as Cup value
    val refs = new Array[Cup](nCups + 1)

    // First cup
    val first = new Cup(value = init.head)
    refs(init.head) = first

    // Generate all numbers to nCups
    val genSeq =
      if (nCups > init.max) init.tail ++ (init.max + 1 to nCups)
      else init.tail

    // Link all the Cups together
    val last = genSeq.foldLeft(first)((prevCup, v) => {
      val newCup = new Cup(value = v, next = null)
      prevCup.next = newCup
      refs(v) = newCup
      newCup
    })

    // and link last to the first one for circular linked list.
    last.next = first

    // Play rounds
    val lastCurrent = (1 to rounds).foldLeft(first)((current, _) => playRound2(current, refs, max = nCups))
    (lastCurrent, refs(1))
  }

  def playRound2(current: Cup, refs: Array[Cup], max: Int = 9 ): Cup = {
    // Remove 3 next cups
    val following = current.next
    val nextCurrent = current.next.next.next.next
    current.next = nextCurrent

    // values  of following 3
    val following3 = readFirstValues(following, 3)

    // Get destination cup
    val destinationValue = findDestinationCup(current.value, following3, max)
    val destinationCup = refs(destinationValue)

    // Insert previous following 3
    following.next.next.next = destinationCup.next
    destinationCup.next = following

    // the next current cup is the fourth
    nextCurrent
  }

  def readFirstValues(cup: Cup, n: Int): List[Int] = {
    (1 to n).foldLeft((cup, List.empty[Int]))((res, _) => {
      val (currentCup, acc) = res
      (currentCup.next, currentCup.value :: acc)
    })._2.reverse
  }

  def findProductOf2StarContainingCups(cup1: Cup):Long =
    readFirstValues(cup1, 3).tail
     .map(_.toLong).product

}
