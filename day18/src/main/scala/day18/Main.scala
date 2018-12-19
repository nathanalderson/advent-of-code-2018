package day18

import scala.io.Source

case class Point(x: Int, y: Int)

sealed trait Acre
case object Trees extends Acre
case object Lumber extends Acre

object Main {
  type State = Map[Point, Acre]

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val state = parse(input)
    println(s"ans1 = ${ans1(state, 50)}")
//    runAndShowForever(state, 50)
    println(s"ans2 = ${ans2(state, 50)}")
  }

  def ans2(state: State, size: Int): Int = {
    val (dupIdx1, dupIdx2) = findDupState(state, 50)
    val cycleSize = dupIdx2 - dupIdx1
    val roundsToRun = dupIdx1 + ((1000000000-dupIdx1) % cycleSize)
    resourceValue(play(state, roundsToRun, size))
  }

  def rounds(state: State, size: Int): Iterator[State] =
    Iterator.iterate(state)(round(size))

  def findDupState(state: State, size: Int): (Int, Int) = {
    val stream = rounds(state, size).toStream
    val (dupState, dupIdx) = stream.zipWithIndex.find {
      case (s, idx) => stream.take(idx).contains(s)
    }.get
    val firstOccurance = stream.indexOf(dupState)
    (firstOccurance, dupIdx)
  }

  def runAndShowForever(state: State, size: Int): Unit =
    rounds(state, size).foreach(s => println(show(s)+"\n"))

  def resourceValue(state: State): Int =
    state.count(_._2==Trees) * state.count(_._2==Lumber)

  def ans1(state: State, size: Int): Int =
    resourceValue(play(state, 10, size))

  def play(state: State, numRounds: Int, size: Int): State =
    rounds(state, size).drop(numRounds).next

  def candidates(size: Int): Seq[Point] =
    for(y <- 0 until size; x <- 0 until size) yield Point(x,y)

  def neighborPoints(p: Point): Seq[Point] =
    (for (x <- p.x-1 to p.x+1; y <- p.y-1 to p.y+1) yield Point(x,y)).filterNot(_ == p)

  def liveNeighbors(p: Point, s: State): Seq[Acre] =
    neighborPoints(p).flatMap(s.get)

  def round(size: Int)(s: State): State =
    candidates(size).flatMap { p =>
      (s.get(p) match {
        case Some(Trees) => if (liveNeighbors(p, s).count(_==Lumber) >= 3) Some(Lumber) else Some(Trees)
        case Some(Lumber) =>
          val ns = liveNeighbors(p, s)
          if (ns.count(_==Lumber) >= 1 && ns.count(_==Trees) >= 1) Some(Lumber) else None
        case _ => if (liveNeighbors(p, s).count(_==Trees) >= 3) Some(Trees) else None
      }).map(p -> _)
    }.toMap

  def parse(input: List[String]): State =
    input.zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.flatMap {
        case ('|', x) => Some(Point(x,y) -> Trees)
        case ('#', x) => Some(Point(x,y) -> Lumber)
        case _ => None
      }
    }.toMap

  def show(state: State): String = {
    val size = state.keys.map(_.x).max
    val board = for(y <- 0 to size) yield {
      val row = for(x <- 0 to size) yield {
        state.get(Point(x,y)) match {
          case Some(Trees) => '|'
          case Some(Lumber) => '#'
          case _ => '.'
        }
      }
      row.mkString
    }
    board.mkString("\n")
  }
}
