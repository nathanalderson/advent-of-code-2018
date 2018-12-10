/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package day09

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {
  List(
    (9, 25, 32),
    (10, 1618, 8317),
    (13, 7999, 146373),
    (17, 1104, 2764),
    (21, 6111, 54718),
    (30, 5807, 37305),
  ).foreach { case (players, lastMarble, highScore) =>
    test (s"play($players, $lastMarble) should be $highScore") {
      Main.play(players, lastMarble) should be (highScore)
      //    println(s"ans1 = ${Main.play(473, 70904)}")
    }
  }

  test("place marble") {
    Main.placeMarble(List(0), 0, 1) should be (List(0,1), 1, 0)
    Main.placeMarble(List(0,1), 1, 2) should be (List(0,2,1), 1, 0)
    Main.placeMarble(List(0,4,2,1,3), 1, 5) should be (List(0,4,2,5,1,3), 3, 0)
  }

  test("place scoring marble") {
    val marbles = List(0,16,8,17,4,18,9,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15)
    val expected = List(0,16,8,17,4,18,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15)
    Main.placeMarble(marbles, 13, 23) should be (expected, 6, 32)
  }
}