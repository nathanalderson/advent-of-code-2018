package day25

import scala.io.Source

case class Point(x: Int, y: Int, z: Int, t: Int)

object Main {
  type Constellation = List[Point]
  type Constellations = List[Constellation]

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val points = parse(input)
    val ans1 = getConstellations(points).length
    println(s"ans1 = $ans1")
    // 626 - too high
  }

  def getConstellations(points: List[Point], constellations: Constellations = List()): Constellations = points match {
    case List() => constellations
    case p :: rest =>
      val (constellation, remaining) = getConstellation(p, rest)
      getConstellations(remaining, constellation::constellations)
  }

  def getConstellation(point: Point, points: List[Point]): (Constellation, List[Point]) =
    getConstellation(points, List(point), List())

  def getConstellation(points: List[Point], constellation: Constellation, leftOvers: List[Point])
  : (Constellation, List[Point]) = points match {
    case List() =>
      (constellation, leftOvers)
    case p :: rest =>
      if (constellation.exists(dist(p, _) <= 3))
        getConstellation(leftOvers ++ rest, p::constellation, List())
      else
        getConstellation(rest, constellation, p::leftOvers)
  }

  def dist(p1: Point, p2: Point): Int =
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) + Math.abs(p1.z - p2.z) + Math.abs(p1.t - p2.t)

  def parse(input: List[String]): List[Point] = {
    val reLine = raw"([-\d]+),([-\d]+),([-\d]+),([-\d]+)".r
    input.map { case reLine(x,y,z,t) => Point(x.toInt, y.toInt, z.toInt, t.toInt) }
  }.toList
}
