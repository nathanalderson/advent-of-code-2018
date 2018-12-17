package day16

import day16.Instruction.Opcode

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

  val allOpcodes: List[Opcode] = List(addr, addi, mulr, muli, banr, bani, borr, bori,
    setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)
}
case class Instruction(opcodeNum: Int, a: Int, b: Int, c: Int)

case class Sample(before: Main.Regs, after: Main.Regs, instruction: Instruction)

object Main {
  type Regs = List[Int]

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val (part1Input, part2Input) = parse(input)
    val ans1 = part1Input.map(candidateOpcodes).count(_.length >= 3)
    println(s"ans1 = $ans1")
    println(s"ans2 = ${ans2(part1Input, part2Input).head}")
  }

  def ans2(part1Input: List[Sample], part2Input: List[Instruction]): Regs = {
    val sampleData = part1Input.map(s => s -> candidateOpcodes(s)).toMap
    val opcodeMapping = determineOpcodes(sampleData, Instruction.allOpcodes)
    part2Input.foldLeft(List(0,0,0,0)) {
      case (regs, inst) => opcodeMapping(inst.opcodeNum)(regs, inst.a, inst.b, inst.c)
    }
  }

  def determineOpcodes(sampleData: Map[Sample, List[Opcode]],
                       unknown: List[Opcode],
                       known: Map[Opcode, Int] = Map())
    : Map[Int, Opcode] =
  {
    if (unknown.isEmpty)
      known.map{ x => x._2 -> x._1 } // flip keys/vals
    else {
      val newlyKnown = sampleData.filter(_._2.length == 1).map {
        case (sample, List(opcode)) => (opcode, sample.instruction.opcodeNum)
      }
      val newSampleData = sampleData.filterNot(_._2.length == 1).mapValues(_.filterNot(newlyKnown.contains))
      if (newlyKnown.nonEmpty)
        determineOpcodes(newSampleData, unknown.filterNot(newlyKnown.contains), known ++ newlyKnown)
      else
        throw new Exception("stuck")
    }
  }

  def candidateOpcodes(sample: Sample): List[Opcode] =
    Instruction.allOpcodes.filter { op =>
      val inst = sample.instruction
      op(sample.before, inst.a, inst.b, inst.c) == sample.after
    }

  def parse(input: Seq[String]): (List[Sample], List[Instruction]) = {
    val break = input.indexOfSlice(List("", "", ""))
    val (part1, part2) = input.splitAt(break)
    val reBefore = raw"Before: \[(\d+), (\d+), (\d+), (\d+)\]".r
    val reAfter = raw"After:  \[(\d+), (\d+), (\d+), (\d+)\]".r
    val reInstruction = raw"(\d+) (\d+) (\d+) (\d+)".r
    val befores = part1.flatMap {
      case reBefore(r0, r1, r2, r3) => Some(List(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
      case _ => None
    }
    val afters = part1.flatMap {
      case reAfter(r0, r1, r2, r3) => Some(List(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
      case _ => None
    }
    val instructions = part1.flatMap {
      case reInstruction(r0, r1, r2, r3) => Some(Instruction(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
      case _ => None
    }
    val samples = befores.zip(afters).zip(instructions).map { case ((b,a),i) => Sample(b,a,i) }.toList
    val program = part2.flatMap {
      case reInstruction(r0, r1, r2, r3) => Some(Instruction(r0.toInt, r1.toInt, r2.toInt, r3.toInt))
      case _ => None
    }.toList
    (samples, program)
  }
}
