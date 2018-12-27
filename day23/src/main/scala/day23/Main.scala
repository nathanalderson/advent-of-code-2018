package day23

import scala.io.Source

case class Point(x: Int, y: Int, z: Int)

object Main {
  type Bot = (Point, Int)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val bots = parse(input)
    val maxBot = bots.maxBy(_._2)
    val inRange = bots.map(bot => distance(bot._1, maxBot._1)).count(_ <= maxBot._2)
    bots.toList.map(bot => bot -> botsInRangeOf(bot, bots))
    bots.collect { case (p, _) => p }
    println(s"ans1 = $inRange")
  }

  def botsInRangeOf(bot: Bot, bots: Map[Point, Int]): Seq[Point] =
    pointsInRangeOf(bot).filter(bots.contains)

  def pointsInRangeOf(bot: Bot): Seq[Point] =
    for {
      x <- (bot._1.x - bot._2) to (bot._1.x + bot._2)
      y <- (bot._1.y - bot._2) to (bot._1.y + bot._2)
      z <- (bot._1.z - bot._2) to (bot._1.z + bot._2)
    } yield {
      Point(x,y,z)
    }

  def distance(p1: Point, p2: Point): Int =
    Math.abs(p2.x - p1.x) + Math.abs(p2.y - p1.y) + Math.abs(p2.z - p1.z)

  def parse(input: List[String]): Map[Point, Int] = {
    val reLine = raw"pos=<([-\d]+),([-\d]+),([-\d]+)>, r=([-\d]+)".r
    input.map {
      case reLine(x, y, z, r) => Point(x.toInt, y.toInt, z.toInt) -> r.toInt
    }.toMap
  }
}
