/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package day11

import day11.Main.Point
import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {
  test("hundredsDigit") {
    Main.hundredsDigit(1) should be (0)
    Main.hundredsDigit(100) should be (1)
    Main.hundredsDigit(1234) should be (2)
  }

  List(
    (Point(3,5), 8, 4),
    (Point(122,79), 57, -5),
    (Point(217,196), 39, 0),
    (Point(101,153), 71, 4),
  ).foreach { case (coord, sn, expected) =>
    test(s"powerLevel($coord, $sn}") {
      Main.powerLevel(sn, coord) should be (expected)
    }
  }

  test("nByNSum") {
    val grid = for(x <- 1 to 300; y <- 1 to 300) yield Point(x,y)
    val powerLevels = grid.map(p => p->Main.powerLevel(18, p)).toMap
    val threeBy = Main.nByNSum(3)(powerLevels, Point(33,45))
    threeBy should be (29)
    val fourBy = Main.nByNSum(4)(powerLevels, Point(33,45))
    fourBy should be (12)
    Main.nByNSum(4)(powerLevels, Point(33,45), Some(threeBy)) should be (fourBy)
  }

  test("highestPowerBlock") {
    val grid = for(x <- 1 to 300; y <- 1 to 300) yield Point(x,y)
    val powerLevels = grid.map(p => p->Main.powerLevel(18, p)).toMap
    Main.highestPowerBlock(powerLevels, Point(90,269)) should be (113, 16)
  }

  test("ans1") {
    Main.ans1(18) should be (Point(33,45), 29)
  }

  test("ans2") {
    Main.ans2(18) should be (Point(90,269), 16, 113)
  }
}
