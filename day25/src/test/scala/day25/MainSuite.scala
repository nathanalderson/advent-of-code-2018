package day25

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Main._

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {

  test("1") {
    val input = """0,0,0,0
                  |3,0,0,0
                  |0,3,0,0
                  |0,0,3,0
                  |0,0,0,3
                  |0,0,0,6
                  |9,0,0,0
                  |12,0,0,0""".stripMargin.lines.toList
    getConstellations(parse(input)).length should be (2)
  }

  test("2") {
    val input = """-1,2,2,0
                  |0,0,2,-2
                  |0,0,0,-2
                  |-1,2,0,0
                  |-2,-2,-2,2
                  |3,0,2,-1
                  |-1,3,2,2
                  |-1,0,-1,0
                  |0,2,1,-2
                  |3,0,0,0""".stripMargin.lines.toList
    getConstellations(parse(input)).length should be (4)
  }

  test("3") {
    val input = """1,-1,0,1
                  |2,0,-1,0
                  |3,2,-1,0
                  |0,0,3,1
                  |0,0,-1,-1
                  |2,3,-2,0
                  |-2,2,0,0
                  |2,-2,0,-1
                  |1,-1,0,-1
                  |3,2,0,2""".stripMargin.lines.toList
    getConstellations(parse(input)).length should be (3)
  }

  test("4") {
    val input = """1,-1,-1,-2
                  |-2,-2,0,1
                  |0,2,1,3
                  |-2,3,-2,1
                  |0,2,3,-2
                  |-1,-1,1,-2
                  |0,-2,-1,0
                  |-2,2,3,-1
                  |1,2,2,0
                  |-1,-2,0,-2""".stripMargin.lines.toList
    getConstellations(parse(input)).length should be (8)
  }

}
