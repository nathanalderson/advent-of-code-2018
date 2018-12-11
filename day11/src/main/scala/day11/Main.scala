/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package day11

object Main {
  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
  }

  def powerLevel(coord: Point, sn: Int): Int = {
    val rackId = coord.x + 10
    hundredsDigit((rackId * 10 + sn) * rackId) - 5
  }

  def hundredsDigit(n: Int): Int = {
    val s = n.toString
    if (s.length >= 3) s(s.length-3).toString.toInt else 0
  }
}
