package day01

class ReportRepair {
  def find2Entries(inputs: List[String]): Long =
    findEntries(inputs, 2)

  def find3Entries(inputs: List[String]): Long =
    findEntries(inputs, 3)

  def findEntries(inputs: List[String], n: Int): Long =
    inputs.map(_.toLong)
      .combinations(n)
      .find(_.sum == 2020)
      .map(_.product)
      .getOrElse(0L)
}
