package day22

import scala.collection.mutable

case class Point(x: Int, y: Int)

sealed trait RegionType
case object Rocky extends RegionType
case object Wet extends RegionType
case object Narrow extends RegionType

sealed trait Equipment
case object Torch extends Equipment
case object ClimbingGear extends Equipment
case object Neither extends Equipment

object Main {
  type State = (Point, Equipment)

  def main(args: Array[String]): Unit = {
    val depth = 3558
    val target = Point(15,740)
    // test values
//    val depth = 510
//    val target = Point(10,10)
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

    def heuristic(s1: State, s2: State): Int =
      Math.abs(s1._1.x - s2._1.x) + Math.abs(s1._1.y - s2._1.y)

    def cost(s1: State, s2: State): Long =
      if (s1 == s2) 0 else if (s1._2 == s2._2) 1 else 7

    def neighbors(s: State): List[State] = s match {
      case (Point(x,y), equip) => List(
        (Point(x+1, y), equip),
        (Point(x-1, y), equip),
        (Point(x, y+1), equip),
        (Point(x, y-1), equip),
        (Point(x,y), Torch),
        (Point(x,y), ClimbingGear),
        (Point(x,y), Neither)
      ).filterNot(_==s).filter(valid)
    }

    def valid(s: State): Boolean = s match {
      case (point@Point(x,y), equip) =>
        (x >= 0 && y >= 0) && (regionType(point) match {
          case Rocky => equip == ClimbingGear || equip == Torch
          case Wet => equip == ClimbingGear || equip == Neither
          case Narrow => equip == Torch || equip == Neither
        })
    }

    println(s"ans1 = ${cave.map(riskLevel).sum}")
    val (cameFrom, costSoFar) = AStar((Point(0,0), Torch), (target, Torch), neighbors, heuristic, cost)
    println(s"ans2 = ${costSoFar((target, Torch))}")
  } // main
}

