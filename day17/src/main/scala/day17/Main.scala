package day17

import scala.annotation.tailrec
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
    }.map(_->Clay).toMap.updated(Point(500,0), Reachable)
  }

  def lowestReachables(board: Board): List[Point] = {
    val reachables = board.filter(_._2 == Reachable).keys
    val maxY = reachables.maxBy(_.y).y
    reachables.filter(_.y == maxY).toList
  }

  def drip(board: Board): Board = {
    lowestReachables(board).foldLeft(board) {
      case (b, p) =>
        val pointBelow = Point(p.x, p.y+1)
        if(inbounds(pointBelow, board) && !board.contains(Point(p.x, p.y+1)))
          drip(board.updated(pointBelow, Reachable))
        else
          board
    }
  }

  def bounds(board: Board): (Point, Point) = {
    val ul = Point(board.keys.minBy(_.x).x, board.keys.minBy(_.y).y)
    val lr = Point(board.keys.maxBy(_.x).x, board.keys.maxBy(_.y).y)
    (ul, lr)
  }

  def inbounds(point: Point, board: Board): Boolean = {
    val (ul, lr) = bounds(board)
    ul.x <= point.x && point.x <= lr.x && ul.y <= point.y && point.y <= lr.y
  }

  def toString(board: Board): String = {
    val (ul, lr) = bounds(board)
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
