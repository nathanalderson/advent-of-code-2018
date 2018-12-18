package day17

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Calendar

import scala.annotation.tailrec
import scala.io.Source

case class Point(x: Int, y: Int)

sealed trait Content
case object Clay extends Content
case object Reachable extends Content
case object Water extends Content


object Main {
  type Board = Map[Point, Content]
  type WorkQueue = Vector[Work]
  type WorkFunction = (Board, Point) => (Board, List[Work])

  case class Work(p: Point, f: WorkFunction)

  lazy val spring = Work(Point(500,0), drip)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val board = parse(input)
    val ans1 = countWaters(run(board))
    println(s"ans1 = $ans1")
    // 348 - too low
  }

  def countWaters(board: Board): Int = {
    val bounds = getBounds(board)
    board.count{ case (p, c) => (c == Water || c == Reachable) && inbounds(bounds)(p) }
  }

  @tailrec
  def run(board: Board, workQueue: WorkQueue = Vector(spring)): Board = {
//    println(s"***** workQueue: ${workQueue.size}")
    val now = Calendar.getInstance().get(Calendar.SECOND)
    if (now%10==0) {
      val writer = new BufferedWriter(new FileWriter(new File(s"out/$now.txt")))
      writer.write(toString(workQueue))
      writer.write(toString(board, ' '))
      writer.close()
    }
    workQueue.headOption match {
      case Some(Work(point, f)) =>
        val (newBoard, newWork) = f(board, point)
        run(newBoard, workQueue.drop(1) ++ newWork)
      case None => board
    }
  }

  def parse(input: List[String]): Board = {
    val reYrange = raw"x=(\d+), y=(\d+)\.\.(\d+)".r
    val reXrange = raw"y=(\d+), x=(\d+)\.\.(\d+)".r
    input.flatMap {
      case reYrange(x,yrange1,yrange2) => (yrange1.toInt to yrange2.toInt).map(Point(x.toInt,_))
      case reXrange(y,xrange1,xrange2) => (xrange1.toInt to xrange2.toInt).map(Point(_,y.toInt))
    }.map(_->Clay).toMap
  }

  val drip: WorkFunction = (board, point) => {
    if(!board.get(point).contains(Reachable)) {
      (board, List())
    } else {
      val (ul, lr) = getBounds(board)
      val pointsToFloor = Iterator.range(point.y + 1, lr.y + 1)
        .map(Point(point.x, _))
        .takeWhile(p => !board.contains(p))
        .toList
      val newBoard = board ++ pointsToFloor.map(_ -> Reachable)
      val newWork = pointsToFloor.lastOption.map(Work(_, spread)).toList
      (newBoard, newWork)
    }
  }

  val spread: WorkFunction = (board, point) => {
    if(!board.get(point).contains(Reachable)) {
      (board, List())
    } else {
      val (fillPoints, fillWith, newWork) = followFloor(point, board) match {
        case (Right(l), Right(r)) => // walls on either side means water
          val respreads = (l ++ r).map(above).filter(p => board.get(p).contains(Reachable)).distinct
          (l ++ r, Water, respreads.map(Work(_, spread)))
        case (Left(l), Right(r)) => (l ++ r, Reachable, l.lastOption.toList.map(Work(_, drip))) // all others means reachable
        case (Right(l), Left(r)) => (l ++ r, Reachable, r.lastOption.toList.map(Work(_, drip)))
        case (Left(l), Left(r)) => (l ++ r, Reachable, (l.lastOption.toList ++ r.lastOption.toList).map(Work(_, drip)))
      }
      val newBoard = board ++ fillPoints.map(_ -> fillWith)
      (newBoard, newWork)
    }
  }

  def leftOf(p: Point) = Point(p.x-1, p.y)
  def rightOf(p: Point) = Point(p.x+1, p.y)
  def below(p: Point) = Point(p.x, p.y+1)
  def above(p: Point) = Point(p.x, p.y-1)

  // these functions return Left[List[Point]] if the points lead off a cliff,
  // or Right[List[Point]] if the points run into a wall
  type FollowResult = Either[List[Point], List[Point]]

  def followFloor(point: Point, board: Board): (FollowResult, FollowResult) =
    (followFloor(leftOf)(point, board), followFloor(rightOf)(point, board))

  def followFloor(dir: Point=>Point)(point: Point, board: Board): FollowResult = {
    val bounds = getBounds(board)
    val points = Iterator.iterate(point)(dir)
      .takeWhile(inbounds(bounds)) // in bounds
      .takeWhile { p => !board.get(p).contains(Clay) } // it's not a wall
      .takeWhile { p => // there's something below us
        val belowP = board.get(below(p))
        belowP.contains(Water) || belowP.contains(Clay)
      }
      .toList
    val result = if (points.isEmpty)
      Left(List()) // can't go this way
    else {
      val oneMore = dir(points.last)
      board.get(oneMore) match {
        case Some(Clay) => Right(points)
        case _ if inbounds(bounds)(oneMore) => Left(points :+ oneMore)
        case _ => Left(points)
      }
    }
    result
  }

  def inbounds(board: Board)(point: Point): Boolean = inbounds(getBounds(board))(point)
  def inbounds(bounds: (Point, Point))(point: Point): Boolean = {
    val (ul, lr) = bounds
    ul.x <= point.x && point.x <= lr.x && ul.y <= point.y && point.y <= lr.y
  }
  def getBounds(board: Board): (Point, Point) = {
    val ul = Point(board.keys.minBy(_.x).x, board.keys.minBy(_.y).y)
    val lr = Point(board.keys.maxBy(_.x).x, board.keys.maxBy(_.y).y)
    (ul, lr)
  }

  def toString(board: Board, fill: String = "."): String = {
    val (ul, lr) = getBounds(board)
    val rows = for (y <- ul.y to lr.y) yield {
      val row = for (x <- ul.x to lr.x) yield {
        board.get(Point(x,y)) match {
          case Some(Clay) => "#"
          case Some(Reachable) => "|"
          case Some(Water) => "~"
          case None => fill
        }
      }
      row.mkString
    }
    rows.mkString("\n")
  }

  def toString(workQueue: WorkQueue): String = {
    workQueue.map {
      case Work(Point(x,y), f) =>
        val func = if (f==drip) "Drip" else "Spread"
        s"$func($x,$y)"
    }.mkString(" ")
  }
}
