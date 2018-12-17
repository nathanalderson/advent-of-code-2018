package day17

import scala.io.Source

case class Point(x: Int, y: Int)

sealed trait Content
case object Clay extends Content
case object Reachable extends Content
case object Water extends Content

object Main {
  type Board = Map[Point, Content]

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val board = parse(input)
    println(toString(board))
  }

  def parse(input: List[String]): Board = {
    val reYrange = raw"x=(\d+), y=(\d+)\.\.(\d+)".r
    val reXrange = raw"y=(\d+), x=(\d+)\.\.(\d+)".r
    input.flatMap {
      case reYrange(x,yrange1,yrange2) => (yrange1.toInt to yrange2.toInt).map(Point(x.toInt,_))
      case reXrange(y,xrange1,xrange2) => (xrange1.toInt to xrange2.toInt).map(Point(_,y.toInt))
    }.map(_->Clay).toMap
  }

  def range(board: Board): (Point, Point) = {
    val ul = Point(board.keys.minBy(_.x).x, board.keys.minBy(_.y).y)
    val lr = Point(board.keys.maxBy(_.x).x, board.keys.maxBy(_.y).y)
    (ul, lr)
  }

  def toString(board: Board): String = {
    val (ul, lr) = range(board)
    val rows = for (y <- ul.y to lr.y) yield {
      val row = for (x <- ul.x to lr.x) yield {
        board.get(Point(x,y)) match {
          case Some(Clay) => "#"
          case Some(Reachable) => "|"
          case Some(Water) => "~"
          case None => " "
        }
      }
      row.mkString
    }
    rows.mkString("\n")
  }
}
