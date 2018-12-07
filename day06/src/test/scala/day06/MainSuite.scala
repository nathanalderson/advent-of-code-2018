/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package day06

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {
  val lines = """Step C must be finished before step A can begin.
                |Step C must be finished before step F can begin.
                |Step A must be finished before step B can begin.
                |Step A must be finished before step D can begin.
                |Step B must be finished before step E can begin.
                |Step D must be finished before step E can begin.
                |Step F must be finished before step E can begin.""".stripMargin.lines.toList

  test("ans1") {
    Main.ans1(lines) should be ("CABDFE")
  }

  test("ans2") {
    Main.ans2(lines, 0, 2) should be ("CABFDE", 15)
  }

  test("step") {
    Main.step(List(Main.Busy("A", 2), Main.Busy("B", 1), Main.Idle)) should be (
      (List(Main.Busy("A", 1), Main.Idle, Main.Idle), List("B"))
    )
  }
}
