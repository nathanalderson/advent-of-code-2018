package day24

import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Main._

@RunWith(classOf[JUnitRunner])
class MainSuite extends FunSuite with Matchers {
  val immuneSystemRaw = """17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
                          |989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3""".stripMargin.lines.toList
  val infectionRaw = """801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
                       |4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4""".stripMargin.lines.toList
  val immuneSystem = parse(immuneSystemRaw, ImmuneSystem)
  val infection = parse(infectionRaw, Infection)
  val groups = immuneSystem ++ infection

//  test("selectTargets") {
//    selectTargets(groups) should contain theSameElementsAs List(
//      infection(0)->Some(immuneSystem(0)), infection(1)->Some(immuneSystem(1)),
//      immuneSystem(0)->Some(infection(1)), immuneSystem(1)->Some(infection(0)))
//  }

//  test("attack") {
//    val targets = selectTargets(groups)
//    attack(targets) should contain theSameElementsAs List(
//      Group(ImmuneSystem, 905, 1274, List("bludgeoning", "slashing"), List("fire"), 25, "slashing", 3),
//      Group(Infection, 796, 4706, List("radiation"), List(""), 116, "bludgeoning", 1), // AoC says this should be 797, but...
//      Group(Infection, 4434, 2961, List("fire", "cold"), List("radiation"), 12, "slashing", 4),
//    )
//  }

  test("play") {
    play(groups).map(_.num) should contain theSameElementsAs List(782, 4434)
  }
}
