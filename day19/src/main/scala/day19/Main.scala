package day19

import scala.io.Source

object Instruction {
  type Opcode = (Main.Regs, Int, Int, Int) => Main.Regs
  def op(f: (Main.Regs, Int, Int)=>Int): Opcode = (regs: Main.Regs, a: Int, b: Int, c: Int) =>
    regs.updated(c, f(regs,a,b))
  def opr(f: (Int, Int)=>Int): Opcode = op((regs, a, b) => f(regs(a), regs(b)))
  def opi(f: (Int, Int)=>Int): Opcode = op((regs, a, b) => f(regs(a), b))

  val addr: Opcode = opr(_+_)
  val addi: Opcode = opi(_+_)
  val mulr: Opcode = opr(_*_)
  val muli: Opcode = opi(_*_)
  val banr: Opcode = opr(_&_)
  val bani: Opcode = opi(_&_)
  val borr: Opcode = opr(_|_)
  val bori: Opcode = opi(_|_)

  val setr: Opcode = opr((a, _) => a)
  val seti: Opcode = op((_, a, _) => a)

  def comp(f: (Main.Regs, Int, Int)=>Boolean): Opcode = op((regs, a, b) => if (f(regs,a,b)) 1 else 0)
  val gtir: Opcode = comp((regs, a, b) => a > regs(b))
  val gtri: Opcode = comp((regs, a, b) => regs(a) > b)
  val gtrr: Opcode = comp((regs, a, b) => regs(a) > regs(b))
  val eqir: Opcode = comp((regs, a, b) => a == regs(b))
  val eqri: Opcode = comp((regs, a, b) => regs(a) == b)
  val eqrr: Opcode = comp((regs, a, b) => regs(a) == regs(b))

  val allOpcodes: Map[String, Opcode] = Map("addr" -> addr, "addi" -> addi, "mulr" -> mulr, "muli" -> muli,
    "banr" -> banr, "bani" -> bani, "borr" -> borr, "bori" -> bori, "setr" -> setr, "seti" -> seti, "gtir" -> gtir,
    "gtri" -> gtri, "gtrr" -> gtrr, "eqir" -> eqir, "eqri" -> eqri, "eqrr" -> eqrr)
}

case class Instruction(op: Instruction.Opcode, a: Int, b: Int, c: Int)

object Main {
  type Regs = List[Int]
  type Program = List[Instruction]

  val emptyRegs = List(0,0,0,0,0,0)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val (program, ipReg) = parse(input)
    val finalRegs = run(program, emptyRegs, ipReg, 0)
    println(s"ans1 = ${finalRegs.head}")
//    run(program, emptyRegs.updated(0,1), ipReg, 0, Some(500), print = true)
    println(s"ans2 = 10915260")
    println(s"""       ans2 calculated by reverse-engineering the input program. See
               |       `decompiled.txt`. Basically it sums the factors of 10551389.""".stripMargin)
  }

  def run(program: Program, regs: Regs, ipReg: Int, ip: Int, iterations: Option[Int] = None, print: Boolean = false): Regs = {
    if (iterations.contains(0))
      regs
    else {
      program.lift(ip) match {
        case Some(inst) =>
          val newRegs = inst.op(regs.updated(ipReg, ip), inst.a, inst.b, inst.c)
          if (print)
            println(f"ip=$ip%2s [${show(regs)}] ${show(inst)} [${show(newRegs)}]")
          val newIp = newRegs(ipReg) + 1
          run(program, newRegs, ipReg, newIp, iterations.map(_-1), print)
        case None => regs
      }
    }
  }

  def parse(input: List[String]): (Program, Int) = {
    val reIp = raw"#ip (\d+)".r
    val reInstruction = raw"(\w+) (\d+) (\d+) (\d+)".r
    val ip = input.head match { case reIp(i) => i.toInt }
    val program = input.tail.map {
      case reInstruction(opcode, a, b, c) => Instruction(Instruction.allOpcodes(opcode), a.toInt, b.toInt, c.toInt)
    }
    (program, ip)
  }

  def show(inst: Instruction): String = {
    val opName = Instruction.allOpcodes.find(_._2 == inst.op).get._1
    s"$opName ${inst.a} ${inst.b} ${inst.c}"
  }

  def show(regs: Regs): String =
    regs.map(r => f"$r%2s").mkString(", ")

}
