package day22

import astar.{AStar, Engine}

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

sealed trait Command
case object Left extends Command
case object Right extends Command
case object Up extends Command
case object Down extends Command
case object EquipTorch extends Command
case object EquipClimbingGear extends Command
case object EquipNeither extends Command


object Main {
  type State = (Point, Equipment)

  def main(args: Array[String]): Unit = {
//    val depth = 3558
//    val target = Point(15,740)
    val depth = 510
    val target = Point(10,10)
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

    val engine: Engine[State, Command] = new Engine[State, Command] {
      def valid(self: State): Boolean = self match {
        case (point@Point(x,y), equip) =>
          (x >= 0 && y >= 0) && (regionType(point) match {
            case Rocky => equip == ClimbingGear || equip == Torch
            case Wet => equip == ClimbingGear || equip == Neither
            case Narrow => equip == Torch || equip == Neither
          })
      }

      def bisimilar(self: State, other: State): Boolean = self == other

      def hash(self: State): Int = self.hashCode

      def transition(state: State, cmd: Command): State = (state, cmd) match {
        case ((Point(x, y), e), Left)    => (Point(x-1, y), e)
        case ((Point(x, y), e), Right)   => (Point(x+1, y), e)
        case ((Point(x, y), e), Up)      => (Point(x, y-1), e)
        case ((Point(x, y), e), Down)    => (Point(x, y+1), e)
        case ((p, e), EquipClimbingGear) => (p, ClimbingGear)
        case ((p, e), EquipTorch)        => (p, Torch)
        case ((p, e), EquipNeither)      => (p, Neither)
      }

      def commands = List(Left, Right, Up, Down, EquipClimbingGear, EquipTorch, EquipNeither)

      def heuristic(s1: State, s2: State): Int =
        Math.abs(s1._1.x - s2._1.x) + Math.abs(s1._1.y - s2._1.y)

      def cost(s1: State, s2: State): Double =
        if (s1._2 == s2._2) 1 else 7

      def distance(s1: State, s2: State): Double = cost(s1, s2) + heuristic(s1, s2)
    }

    val astarSolver = AStar[State, Command](
      (Point(0,0), Torch),
      (target, Torch),
      engine
    )

    def pathLen(path: List[Command]): Int = {
      path.map { command =>
        if (command == EquipNeither || command == EquipTorch || command == EquipClimbingGear) 7
        else 1
      }.sum
    }

    println(s"ans1 = ${cave.map(riskLevel).sum}")
    val path = astarSolver.computePath.get
    println(s"path = $path")
    println(s"ans2 = ${pathLen(path)}")

  } // main
}

