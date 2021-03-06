/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package day19

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {
  val (program, ipReg) = Main.parse("""#ip 0
                                      |seti 5 0 1
                                      |seti 6 0 2
                                      |addi 0 1 0
                                      |addr 1 2 3
                                      |setr 1 0 0
                                      |seti 8 0 4
                                      |seti 9 0 5""".stripMargin.lines.toList)
  test("") {
    Main.run(program, Main.emptyRegs, ipReg, 0) should be (List(6,5,6,0,0,9))
  }
}
