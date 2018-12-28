package day23

import scala.collection.mutable
import scala.io.Source

case class Point(x: Int, y: Int, z: Int) {
  def apply(i: Int): Int = toList.apply(i)
  def toList: List[Int] = List(x,y,z)
}
case class Bot(pos: Point, r: Int)

object Main {
  type Box = (Point, Point) // min corner, max corner

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val bots = parse(input)
    val maxBot = bots.maxBy(_.r)
    val inRange = bots.map(bot => distance(bot.pos, maxBot.pos)).count(_ <= maxBot.r)
    println(s"ans1 = $inRange")

    val box = initialBox(bots)
    // sort by most in range, then largest box, then closest to origin
    val ordering: Ordering[(Int, Int, Int, Box)] = Ordering.by[(Int, Int, Int, Box), (Int, Int, Int)](x => (x._1, x._2, -1*x._3))
    val queue = mutable.PriorityQueue[(Int, Int, Int, Box)](
      (bots.length, 2*box._2.x, 3*box._2.x, box))(ordering) // inRange, boxSize, distToOrigin, box
    val ans2 = processQueue(queue, bots)
    println(s"ans2 = $ans2")
  }

  def processQueue(queue: mutable.PriorityQueue[(Int, Int, Int, Box)], bots: List[Bot]): Int = {
    while (queue.nonEmpty) {
      val (inRange, boxSize, distToOrigin, box) = queue.dequeue()
      if (boxSize == 1) {
        println(s"Found closest at ${box._1} dist $distToOrigin ($inRange bots in Range)")
        return distToOrigin
      }
      val newBoxSize = boxSize / 2
      for (octant <- List((0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1), (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1))) {
        val newBoxMin = Point(box._1.x + newBoxSize * octant._1, box._1.y + newBoxSize * octant._2, box._1.z + newBoxSize * octant._3)
        val newBoxMax = Point(newBoxMin.x+newBoxSize, newBoxMin.y+newBoxSize, newBoxMin.z+newBoxSize)
        val newBox = (newBoxMin, newBoxMax)
        val newInRange = numIntersections(newBox, bots)
        queue += ((newInRange, newBoxSize, boxDistanceToOrigin(newBox), newBox))
      }
    }
    throw new Exception("No solution found")
  }

  // get the smallest, origin-centered, power-of-two-sized box which contains every point in range of any bot
  def initialBox(bots: List[Bot]): Box = {
    val maxDist = bots.map(b => b.pos.toList.map(Math.abs(_) + b.r).max).max
    val size = Iterator.iterate(1)(_ * 2).dropWhile(_ <= maxDist).next
    (Point(-size, -size, -size), Point(size, size, size))
  }

  def intersectsBox(box: Box, bot: Bot): Boolean = {
    val d = List(0,1,2).foldLeft(0) { (d, i) =>
      val (boxlow, boxhigh) = (box._1(i), box._2(i) - 1)
      val d2 = d + (Math.abs(bot.pos(i) - boxlow) + Math.abs(bot.pos(i) - boxhigh))
      d2 - (boxhigh - boxlow)
    }
    (d / 2) <= bot.r
  }

  def numIntersections(box: Box, bots: List[Bot]): Int =
    bots.count(intersectsBox(box, _))

  def boxDistanceToOrigin(box: Box): Int = {
    val corners = for {
      x <- List(box._1.x, box._2.x)
      y <- List(box._1.y, box._2.y)
      z <- List(box._1.z, box._2.z)
    } yield Point(x,y,z)
    corners.map(distance(_, Point(0,0,0))).min
  }

  def distance(p1: Point, p2: Point): Int =
    Math.abs(p2.x - p1.x) + Math.abs(p2.y - p1.y) + Math.abs(p2.z - p1.z)

  def parse(input: List[String]): List[Bot] = {
    val reLine = raw"pos=<([-\d]+),([-\d]+),([-\d]+)>, r=([-\d]+)".r
    input.map {
      case reLine(x, y, z, r) => Bot(Point(x.toInt, y.toInt, z.toInt), r.toInt)
    }
  }
}
