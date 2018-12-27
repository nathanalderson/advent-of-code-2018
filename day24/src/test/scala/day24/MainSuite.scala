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
  val infection = parse(infectionRaw, Infection, immuneSystem.keys.max + 1)
  val groups = immuneSystem ++ infection

  test("selectTargets") {
    selectTargets(immuneSystem.toList, infection.toList) should contain theSameElementsAs List(
      1->Some(2), 0->Some(3),
      3->Some(1), 2->Some(0))
  }

  test("attack") {
    val targets = selectTargets(immuneSystem.toList, infection.toList)
    attack(groups, targets).mapValues(_.num) should be (Map(1 -> 905, 2 -> 797, 3 -> 4434))
  }

  test("play") {
    play(groups).mapValues(_.num) should be (Map(2->782, 3->4434))
  }
}
