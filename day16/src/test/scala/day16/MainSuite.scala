package day16

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {
  test("instructions") {
    val regs = Main.mkRegs(1,2,3,4)
    import Instruction._
    addr(regs, 0, 1, 3) should be (Main.mkRegs(1,2,3,3))
    muli(regs, 0, 1, 3) should be (Main.mkRegs(1,2,3,1))
    setr(regs, 0, 1, 3) should be (Main.mkRegs(1,2,3,1))
    seti(regs, 0, 1, 3) should be (Main.mkRegs(1,2,3,0))
    gtir(regs, 0, 1, 3) should be (Main.mkRegs(1,2,3,0))
    gtir(regs, 9, 1, 3) should be (Main.mkRegs(1,2,3,1))
  }

  test("candidateOpcodes") {
    import Instruction._
    val sample = Sample(Main.mkRegs(3,2,1,1), Main.mkRegs(3,2,2,1), Instruction(9,2,1,2))
    Main.candidateOpcodes(sample) should contain theSameElementsAs List(mulr, addi, seti)
  }

  test("parsePart1") {
    val input = """|Before: [3, 2, 1, 1]
                   |9 2 1 2
                   |After:  [3, 2, 2, 1]""".stripMargin.lines.toList
    Main.parsePart1(input) should have length 1
  }
}
