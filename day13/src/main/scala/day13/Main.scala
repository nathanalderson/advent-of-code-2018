package day13

import scala.io.Source

case class Point(x:Int, y:Int)

sealed trait Track
object Track {
  def fromChar(c: Char): Option[Track] = {
    if (List('|', '-', 'v', '>', '<', '^').contains(c)) Some(Straight)
    else if (c == '/') Some(Curve('/'))
    else if (c == '\\') Some(Curve('\\'))
    else if (c == '+') Some(Intersection)
    else None
  }
}
case object Straight extends Track
case class Curve(c: Char) extends Track
case object Intersection extends Track

sealed trait Direction
object Direction {
  def fromChar(c: Char): Option[Direction] = c match {
    case '^' => Some(N)
    case '>' => Some(E)
    case 'v' => Some(S)
    case '<' => Some(W)
    case _ => None
  }
}
case object N extends Direction
case object E extends Direction
case object S extends Direction
case object W extends Direction

sealed trait TurnDir
case object L extends TurnDir
case object R extends TurnDir
case object Str extends TurnDir
case class Cart(pos: Point, dir: Direction, nextTurn: TurnDir = L)
object Cart {
  implicit def ordering: Ordering[Cart] = Ordering.by(c => (c.pos.y, c.pos.x))
}

object Main {
  type Board = Map[Point, Track]
  type Carts = List[Cart]
  type Collisions = List[Point]

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val (board, carts) = parse(input)
    println(s"ans1 = ${firstCollision(board, carts)}")
    println(s"ans2 = ${lastCart(board, carts)}")
  }

  def firstCollision(board: Board, carts: Carts): Point =
    ticks(board, carts).map(_._2).dropWhile(_.isEmpty).next.head

  def ticks(board: Board, carts: Carts): Iterator[(Carts, List[Point])] =
    Iterator.iterate((carts, List[Point]())) {
      case (cs, _) => tick(board)(cs)
    }

  def lastCart(board: Board, carts: Carts): Point =
    ticks(board, carts).map(_._1).dropWhile(_.length>1).next.head.pos

  // returns updated carts and a list of collision locations
  def tick(board: Board)(cartsToMove: Carts,
                         cartsDoneMoving: Carts = List(),
                         collisions: Collisions = List())
    : (Carts, Collisions) =
  cartsToMove match {
    case List() => (cartsDoneMoving, collisions)
    case cart::rest =>
      val (newCart, newCollision) = move(board, cartsDoneMoving++rest)(cart)
      val (remainingCarts, doneCarts) = newCollision match {
        case None => (rest, newCart::cartsDoneMoving)
        case Some(pos) => (rest.filterNot(_.pos == pos), cartsDoneMoving.filterNot(_.pos == pos))
      }
      tick(board)(remainingCarts, doneCarts.sorted, collisions++newCollision)
  }

  def parse(input: String): (Board, Carts) = (parseBoard(input), parseCarts(input))

  def parseBoard(input: String): Board =
    input.lines.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case (c, x) => Track.fromChar(c).map { Point(x, y) -> _ }
      }
    }.toMap

  def parseCarts(input: String): Carts =
    input.lines.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case (c, x) => Direction.fromChar(c).map { dir => Cart(Point(x,y), dir) }
      }
    }.toList.sorted

  def nextTurnDir(td: TurnDir): TurnDir = td match {
    case L => Str
    case Str => R
    case R => L
  }

  def curve(d: Direction, c: Curve): Direction = c match {
    case Curve('/') => d match {
      case N => E; case E => N; case S => W; case W => S
    }
    case Curve('\\') => d match {
      case N => W; case E => S; case S => E; case W => N
    }
    case _ => throw new Exception("bad curve")
  }

  def turn(d: Direction, td: TurnDir): Direction = td match {
    case Str => d
    case L => d match {
      case N => W; case E => N; case S => E; case W => S
    }
    case R => d match {
      case N => E; case E => S; case S => W; case W => N
    }
  }

  // returns updated cart and an optional collision location
  def move(board: Board, carts: Carts)(cart: Cart): (Cart, Option[Point]) = {
    val newCart = board(cart.pos) match {
      case Straight => Cart(dirOf(cart.pos, cart.dir), cart.dir, cart.nextTurn)
      case c:Curve =>
        val newDir = curve(cart.dir, c)
        Cart(dirOf(cart.pos, newDir), newDir, cart.nextTurn)
      case Intersection =>
        val newDir = turn(cart.dir, cart.nextTurn)
        Cart(dirOf(cart.pos, newDir), newDir, nextTurnDir(cart.nextTurn))
    }
    (newCart, collisions(newCart, carts))
  }

  def collisions(cart: Cart, carts: Carts): Option[Point] = carts.find(_.pos == cart.pos).map(_.pos)

  def dirOf(p: Point, dir: Direction): Point = dir match {
    case N => Point(p.x, p.y-1)
    case S => Point(p.x, p.y+1)
    case E => Point(p.x+1, p.y)
    case W => Point(p.x-1, p.y)
  }
}
