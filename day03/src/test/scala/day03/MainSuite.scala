/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package day03

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainSuite extends WordSpec with Matchers {
  val claims = List(Claim(1, 1,3, 4,4), Claim(2, 3,1, 4,4), Claim(3, 5,5, 2,2))

  "Claim" should {
    "convert itself to locations" in {
      val c = Claim(0, 1,1, 1,1)
      c.getLocs() should contain theSameElementsAs List(Loc(1,1))
    }

    "build from a string" in {
      Claim("#1 @ 2,3: 4x5") should be (Claim(1, 3,2, 5,4))
    }
  }

  "overlaps" should {
    "compute correct number of overlaps" in {
      Main.overlaps(claims).size should be (4)
    }
  }

  "no overlaps" should {
    "return the claims with no overlaps" in {
      Main.no_overlaps(claims).map(_.id) should contain theSameElementsAs List(3)
    }
  }
}
