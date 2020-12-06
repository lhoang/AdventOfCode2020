package day06

import utils.InputUtils.splitInput


object Customs {

  def countDistinctAnswers(input: List[String]): Int = {
    splitInput(input)
      .map(_.mkString.toSet.size)
      .sum
  }

  def countAllYesAnswers(input: List[String]): Int =
    splitInput(input)
      .map(answers => answers.mkString
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .count(_._2 == answers.size))
      .sum
}
