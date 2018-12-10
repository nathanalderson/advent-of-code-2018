/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package day10

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Main {
  type Board = Map[(Long, Long), (Long, Long)]
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val board = parse(input)
    val (convergence, idx) = Iterator.iterate(board)(step)
      .zipWithIndex
      .dropWhile { case (b,_) => area(b) > 2000 }
      .take(5)
      .minBy { case (b,_) => area(b) }
    println(s"step: $idx")
    show(convergence)
  }

  def parse(input: List[String]): Board = {
    val num = raw"[-0-9]+"
    val re = raw"position=<\s*($num)\s*,\s*($num)\s*> velocity=<\s*($num)\s*,\s*($num)\s*>".r
    input.map {
      case re(x,y,dx,dy) => ((x.toLong,y.toLong), (dx.toLong,dy.toLong))
      case _ => throw new Exception("malformed line")
    }.toMap
  }

  def step(board: Board): Board =
    board.map { case (p, v) => (p._1+v._1, p._2+v._2) -> v }

  def bounds(board: Board): (Long, Long, Long, Long) = {
    val minx = board.keys.map(_._1).min
    val miny = board.keys.map(_._2).min
    val maxx = board.keys.map(_._1).max
    val maxy = board.keys.map(_._2).max
    (minx, miny, maxx, maxy)
  }

  def area(board: Board): Long = {
    val (minx, miny, maxx, maxy) = bounds(board)
    (maxx-minx) * (maxy-miny)
  }

  def show(board: Board): Unit = {
    val (minx, miny, maxx, maxy) = bounds(board)
    for (y <- miny to maxy) {
      for (x <- minx to maxx) {
        if (board.contains((x,y))) print("X") else print(" ")
      }
      print("\n")
    }
    print("\n")
  }
}
