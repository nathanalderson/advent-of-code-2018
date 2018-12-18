package day17

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

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val board = parse(input)
    val ans1 = countWaters(run(board))
    println(s"ans1 = $ans1")
    // 348 - too low
  }

  def countWaters(board: Board): Int = {
    val bounds = getBounds(board)
    val waters = board.filter { case (p, c) => (c == Water || c == Reachable || c == Reachable) && inbounds(bounds)(p) }
    waters.size - 1 // -1 for the spring
  }

  def run(board: Board, workQueue: WorkQueue): Board = {
    val lastBoard = Iterator.iterate(board)(round)
      .sliding(2)
      .dropWhile { case Seq(b1, b2) => b1 != b2 }
      .next
      .head
//    println(toString(lastBoard, ' '))
    lastBoard
  }

  def round(board: Board): Board = spread(drip(board))

  def parse(input: List[String]): Board = {
    val reYrange = raw"x=(\d+), y=(\d+)\.\.(\d+)".r
    val reXrange = raw"y=(\d+), x=(\d+)\.\.(\d+)".r
    input.flatMap {
      case reYrange(x,yrange1,yrange2) => (yrange1.toInt to yrange2.toInt).map(Point(x.toInt,_))
      case reXrange(y,xrange1,xrange2) => (xrange1.toInt to xrange2.toInt).map(Point(_,y.toInt))
    }.map(_->Clay).toMap.updated(Point(500,0), Reachable)
  }

  val drip: WorkFunction = (board, point) => {
    val (ul, lr) = getBounds(board)
    val pointsToFloor = (point.y+1 to lr.y).map(Point(point.x, _)).takeWhile(p => !board.contains(p))
    val newBoard = board ++ pointsToFloor.map(_ -> Reachable)
    val newWork = pointsToFloor.lastOption.map(Work(_, spread)).toList
    (newBoard, newWork)
  }

  val spread: WorkFunction = (board, point) => {
    val (fillPoints, fillWith, newWork) = followFloor(point, board) match {
      case (Right(l), Right(r)) => // walls on either side means water
        val respreads = (l++r).map(above).filter(p => board.get(p).contains(Reachable))
        println(s"respreads: $respreads")
        (l++r, Water, respreads.map(Work(_, spread)))
      case (Left(l), Right(r)) => (l++r, Reachable, l.headOption.toList.map(Work(_, drip))) // all others means reachable
      case (Right(l), Left(r)) => (l++r, Reachable, r.lastOption.toList.map(Work(_, drip)))
      case (Left(l), Left(r)) => (l++r, Reachable, (l.headOption.toList++r.lastOption.toList).map(Work(_, drip)))
    }
    val newBoard = board ++ fillPoints.map(_ -> fillWith)
    (newBoard, newWork)
  }

  def getNewlyReachable(board: Board, convertToReachable: Boolean = true): (Board, List[Point]) = {
    val newReachables = board.filter(_._2 == Reachable).keys.toList.sortBy(_.y * -1)
    val newBoard = if (convertToReachable)
      board.mapValues {
        case Reachable => Reachable
        case other => other
      }
    else board
    (newBoard, newReachables)
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

  def toString(board: Board, fill: Char = '.'): String = {
    val (ul, lr) = getBounds(board)
    val rows = for (y <- ul.y to lr.y) yield {
      val row = for (x <- ul.x to lr.x) yield {
        board.get(Point(x,y)) match {
          case Some(Clay) => "#"
          case Some(Reachable) => "/"
          case Some(Reachable) => "|"
          case Some(Water) => "~"
          case None => "."
        }
      }
      row.mkString
    }
    rows.mkString("\n")
  }
}
