package day22

import scalaz.Memo

import scala.collection.mutable

case class Point(x: Int, y: Int)

sealed trait RegionType
case object Rocky extends RegionType
case object Wet extends RegionType
case object Narrow extends RegionType

object Main {

  def main(args: Array[String]): Unit = {
    val depth = 3558
    val target = Point(15,740)
    val cave = for (x <- 0 to target.x; y <- 0 to target.y) yield Point(x,y)
    val erosionLevels = mutable.HashMap[Point, Long]()
    val geoIdxs = mutable.HashMap[Point, Long]()

    def geoIdx(point: Point): Long = {
      geoIdxs.get(point) match {
        case Some(idx) => idx
        case None =>
          val idx = point match {
            case Point(0, 0) => 0
            case `target` => 0
            case Point(x, 0) => x * 16807
            case Point(0, y) => y * 48271
            case Point(x, y) => erosionLevel(Point(x - 1, y)) * erosionLevel(Point(x, y - 1))
          }
          geoIdxs.update(point, idx)
          idx
      }
    }

    def erosionLevel(point: Point): Long = {
      erosionLevels.get(point) match {
        case Some(level) => level
        case None =>
          val level = (depth + geoIdx(point)) % 20183
          erosionLevels.update(point, level)
          level
      }
    }

    def regionType(point: Point): RegionType = erosionLevel(point) % 3 match {
      case 0 => Rocky
      case 1 => Wet
      case 2 => Narrow
    }

    def riskLevel(point: Point): Long = regionType(point) match {
      case Rocky => 0
      case Wet => 1
      case Narrow => 2
    }

    def show(cave: IndexedSeq[Point]): Unit = {
      val rows = (0 to target.y).map { y =>
        val row = (0 to target.x).map { x =>
          regionType(Point(x,y)) match {
            case Rocky => '.'
            case Wet => '='
            case Narrow => '|'
          }
        }
        row.mkString
      }
      println(rows.mkString("\n"))
    }

//    show(cave)
    val totalRisk = cave.map(riskLevel).sum
    println(s"ans1 = $totalRisk")
    // 11785 - too low
  }

}

