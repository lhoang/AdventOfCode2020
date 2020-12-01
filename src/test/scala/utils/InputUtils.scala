package utils

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
}
