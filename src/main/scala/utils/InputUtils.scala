package utils

import scala.annotation.tailrec
import scala.io.Source

object InputUtils {

  /**
   * Read a file from the test-resources folder.
   * @param filename  ex: "test.txt" for /test/resources/test.txt
   * @return Each line as String
   */
  def read(filename: String): List[String] = {
    val source = Source.fromURL(getClass.getResource('/' + filename))
    val lines = source.getLines().toList
    source.close
    lines
  }

  /**
   * Split the input every blank line.
   * @param input List of String from #read
   * @return List of List of String
   */
  def splitInput(input: List[String]): List[List[String]] = {
    @tailrec
    def rec(remaining: List[String], result: List[List[String]]): List[List[String]] = {
      remaining match {
        case last :: Nil => (last :: result.head) :: result.tail
        case head :: tail if head.isEmpty => rec(tail, List() :: result)
        case head :: tail => rec(tail, (head :: result.head) :: result.tail)
        case _ => result
      }
    }
    val split = rec(input, List(List()))
    split.map(_.reverse).reverse
  }
}
