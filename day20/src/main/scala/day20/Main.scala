package day20

import scala.io.Source

case class Point(x: Int, y: Int)

object Main {
  type Graph = Map[Point, Set[Point]] // which points are reachable from each point

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
  }

  def follow(path: String): Graph = follow(path.toList, List(Point(0,0)), Map())

  def follow(path: List[Char], pos: List[Point], graph: Graph): Graph = {
    path match {
      case List() => graph
      case 'N'::rest => follow(rest, pos.map(north), addReachable(graph, pos, north))
      case 'S'::rest => follow(rest, pos.map(south), addReachable(graph, pos, south))
      case 'E'::rest => follow(rest, pos.map(east), addReachable(graph, pos, east))
      case 'W'::rest => follow(rest, pos.map(west), addReachable(graph, pos, west))
      case '('::rest => branch(rest, pos, graph)
    }
  }

  def branch(path: List[Char], pos: List[Point], graph: Graph): Graph = {
    path match {
      case List() => graph
      case 'N'::_ => follow(path, pos, graph)
      case 'S'::_ => follow(path, pos, graph)
      case 'E'::_ => follow(path, pos, graph)
      case 'W'::_ => follow(path, pos, graph)
      case '|'::rest =>
      case ')'::rest => follow(rest, pos, graph)
    }
  }

  def addReachable(graph: Graph, points: List[Point], direction: Direction): Graph = {
    points.foldLeft(graph) { (graph, p1) =>
      val p2 = direction(p1)
      graph.updated(p1, graph.getOrElse(p1, Set()).+(p2))
           .updated(p2, graph.getOrElse(p2, Set()).+(p1))
    }
  }

  type Direction = Point => Point
  def north(point: Point): Point = Point(point.x, point.y+1)
  def south(point: Point): Point = Point(point.x, point.y-1)
  def east(point: Point): Point = Point(point.x+1, point.y)
  def west(point: Point): Point = Point(point.x-1, point.y)
}

