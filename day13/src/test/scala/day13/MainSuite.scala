package day13

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {
  test("parseBoard") {
    Main.parseBoard("/-+\n^\n|") should be (Map(Point(0,0)->Curve('/'), Point(1,0)->Straight, Point(2,0)->Intersection,
      Point(0,1)->Straight, Point(0,2)->Straight))
  }

  test("parseCarts") {
    Main.parseCarts("/-><\n^\nv") should contain theSameElementsAs List(Cart(Point(2,0), E), Cart(Point(3,0), W),
      Cart(Point(0,1), N), Cart(Point(0,2), S))
  }

  List(
    (">--", (Cart(Point(2,0), E, L), None)),
    (">->", (Cart(Point(2,0), E, L), Some(Point(2,0)))),
    (">\\", (Cart(Point(1,1), S, L), None)),
    (">+", (Cart(Point(1,-1), N, Str), None)),
  ).foreach { case (input, expected) =>
    test(s"move $input") {
      val (board, carts) = Main.parse(input)
      val (newCart, _) = Main.move(board, carts)(carts.head)
      Main.move(board, carts)(newCart) should be (expected)
    }
  }

  List(
    (">-", (List(Cart(Point(1,0), E, L)), List())),
    ("><", (List(), List(Point(1,0)))),
    (">-<", (List(), List(Point(1,0)))),
    (">>", (List(), List(Point(1,0)))),
    (">->", (List(Cart(Point(1,0), E, L), Cart(Point(3,0), E, L)), List())),
  ).foreach { case (input, (expectedCarts, expectedCollisions)) =>
    test(s"tick $input") {
      val (board, carts) = Main.parse(input)
      val (newCarts, collisions) = Main.tick(board)(carts)
      newCarts should contain theSameElementsAs expectedCarts
      collisions should contain theSameElementsAs expectedCollisions
    }
  }

  test(s"cart ordering") {
    List(Point(1,1), Point(0,1), Point(1,0), Point(0,0)).map(Cart(_,N)).sorted should be (
      List(Cart(Point(0,0),N,L), Cart(Point(1,0),N,L), Cart(Point(0,1),N,L), Cart(Point(1,1),N,L)))
  }

}
