package day24

sealed trait Team
case object ImmuneSystem extends Team
case object Infection extends Team

object Input {
  val immuneSystem =
    """5711 units each with 6662 hit points (immune to fire; weak to slashing) with an attack that does 9 bludgeoning damage at initiative 14
      |2108 units each with 8185 hit points (weak to radiation, bludgeoning) with an attack that does 36 slashing damage at initiative 13
      |1590 units each with 3940 hit points with an attack that does 24 cold damage at initiative 5
      |2546 units each with 6960 hit points with an attack that does 25 slashing damage at initiative 2
      |1084 units each with 3450 hit points (immune to bludgeoning) with an attack that does 27 slashing damage at initiative 11
      |265 units each with 8223 hit points (immune to radiation, bludgeoning, cold) with an attack that does 259 cold damage at initiative 12
      |6792 units each with 6242 hit points (immune to slashing; weak to bludgeoning, radiation) with an attack that does 9 slashing damage at initiative 18
      |3336 units each with 12681 hit points (weak to slashing) with an attack that does 28 fire damage at initiative 6
      |752 units each with 5272 hit points (immune to slashing; weak to bludgeoning, radiation) with an attack that does 69 radiation damage at initiative 4
      |96 units each with 7266 hit points (immune to fire) with an attack that does 738 bludgeoning damage at initiative 8""".stripMargin.lines.toList
  val infection =
    """1492 units each with 47899 hit points (weak to fire, slashing; immune to cold) with an attack that does 56 bludgeoning damage at initiative 15
      |3065 units each with 39751 hit points (weak to bludgeoning, slashing) with an attack that does 20 slashing damage at initiative 1
      |7971 units each with 35542 hit points (weak to bludgeoning, radiation) with an attack that does 8 bludgeoning damage at initiative 10
      |585 units each with 5936 hit points (weak to cold; immune to fire) with an attack that does 17 slashing damage at initiative 17
      |2449 units each with 37159 hit points (immune to cold) with an attack that does 22 cold damage at initiative 7
      |8897 units each with 6420 hit points (immune to bludgeoning, slashing, fire; weak to radiation) with an attack that does 1 bludgeoning damage at initiative 19
      |329 units each with 31704 hit points (weak to fire; immune to cold, radiation) with an attack that does 179 bludgeoning damage at initiative 16
      |6961 units each with 11069 hit points (weak to fire) with an attack that does 2 radiation damage at initiative 20
      |2837 units each with 29483 hit points (weak to cold) with an attack that does 20 bludgeoning damage at initiative 9
      |8714 units each with 7890 hit points with an attack that does 1 cold damage at initiative 3""".stripMargin.lines.toList
}

case class Group(team: Team, num: Int, hp: Int, weakTo: List[String], immuneTo: List[String], attack: Int, attackType: String, initiative: Int) {
  def effectivePower: Int = num * attack
  def effectiveDamage(damage: Int, damageType: String): Int =
    if (weakTo.contains(damageType)) damage * 2
    else if (immuneTo.contains(damageType)) 0
    else damage
  def takeDamage(damage: Int, damageType: String): Option[Group] = {
    val numKilled = effectiveDamage(damage, damageType) / hp
    if (numKilled >= num) None
    else Some(this.copy(num = num - numKilled))
  }
  override def toString: String = s"$team<$num, $hp>"
}

object Main {
  def main(args: Array[String]): Unit = {
    val immuneSystem = parse(Input.immuneSystem, ImmuneSystem)
    val infection = parse(Input.infection, Infection, immuneSystem.keys.max + 1)
    val allGroups = immuneSystem ++ infection
    val finalGroups = play(allGroups)
    val ans1 = finalGroups.values.map(_.num).sum
    println(s"ans1 = $ans1")
    // 27 => infection win
    // 28-33 => stalemate (causes my program to hang...)
    // 34 => immune system win
    val boost = 34
    val boostedImmuneSystem = immuneSystem.mapValues(g => g.copy(attack=g.attack+boost))
    val finalGroupsBoosted = play(boostedImmuneSystem ++ infection)
    val winner = finalGroupsBoosted.head._2.team
    println(s"winner with boost $boost is $winner")
    println(s"number of winning units: ${finalGroupsBoosted.values.map(_.num).sum}")
  }

  def play(groups: Map[Int, Group]): Map[Int, Group] = {
//    println("***** round")
    val (immuneSystem, infection) = groups.toList.partition(_._2.team == ImmuneSystem)
    if (immuneSystem.isEmpty || infection.isEmpty)
      groups
    else {
      val result = attack(groups, selectTargets(immuneSystem, infection))
//      println(s"result: $result")
      play(result)
    }
  }

  def attack(groups: Map[Int, Group], groupTargets: Map[Int, Option[Int]]): Map[Int, Group] = {
    implicit val ordering: Ordering[Group] = Ordering.by[Group, Int](_.initiative).reverse
    val (killedGroups, remainingGroups) = groups.toList.sortBy(_._2).map(_._1).foldLeft(List[Int](), groups) {
      case ((killed, remaining), attackingGroupIdx) =>
        if (killed.contains(attackingGroupIdx))
          (killed, remaining)
        else {
          groupTargets(attackingGroupIdx) match {
            case None => (killed, remaining)
            case Some(targetGroupIdx) =>
              val attackingGroup = remaining(attackingGroupIdx)
              val targetGroup = remaining(targetGroupIdx)
//              println(s"$attackingGroup attacking $targetGroup")
              targetGroup.takeDamage(attackingGroup.effectivePower, attackingGroup.attackType) match {
                case None => (targetGroupIdx::killed, remaining)
                case Some(t) => (killed, remaining.updated(targetGroupIdx, t))
              }
          }
        }
    }
    remainingGroups.filterNot(g => killedGroups.contains(g._1))
  }

  def selectTargets(immuneSystem: List[(Int, Group)], infection: List[(Int, Group)]): Map[Int, Option[Int]] =
    selectTargetsHelper(immuneSystem, infection) ++ selectTargetsHelper(infection, immuneSystem)

  def selectTargetsHelper(groups: List[(Int, Group)], opposingGroups: List[(Int, Group)]): Map[Int, Option[Int]] = {
    implicit val ordering: Ordering[Group] = Ordering.by[Group, (Int, Int)] { g =>
      (g.effectivePower, g.initiative)
    }.reverse
    groups.sortBy(_._2).foldLeft(List[(Int, Option[Int])](), opposingGroups) {
      case ((soFar, remainingOpponents), (idx, g)) =>
        val (chosenOpponentIdx, remaining) = selectTarget((idx, g), remainingOpponents)
        ((idx, chosenOpponentIdx)::soFar, remaining)
    }._1.toMap
  }

  def selectTarget(group: (Int, Group), opposingGroups: List[(Int, Group)]): (Option[Int], List[(Int, Group)]) = {
    implicit val ordering: Ordering[Group] = Ordering.by[Group, (Int, Int, Int)] { g =>
      (g.effectiveDamage(group._2.attack, group._2.attackType), g.effectivePower, g.initiative)
    }.reverse
    opposingGroups.sortBy(_._2).headOption match {
      case None => (None, opposingGroups)
      case Some((idx, g)) =>
        if (g.effectiveDamage(group._2.attack, group._2.attackType) > 0) (Some(idx), opposingGroups.filterNot(_._1 == idx))
        else (None, opposingGroups)
    }
  }

  def parse(input: List[String], team: Team, startIdx: Int = 0): Map[Int, Group] = {
    val reGroup = raw"(\d+) units each with (\d+) hit points (\(.*?\))? ?with an attack that does (\d+) (\w+) damage at initiative (\d+)".r
    val reImmuneTo = raw"immune to ([\w, ]+)".r.unanchored
    val reWeakTo = raw"weak to ([\w, ]+)".r.unanchored
    input.zipWithIndex.map {
      case (reGroup(num, hp, attack, attackType, initiative), idx) =>
        idx+startIdx -> Group(team, num.toInt, hp.toInt, List(), List(), attack.toInt, attackType, initiative.toInt)
      case (reGroup(num, hp, weakAndImmune, attack, attackType, initiative), idx) =>
        val weakTo = weakAndImmune match { case reWeakTo(l) => l.split(", ").toList; case _ => List[String]() }
        val immuneTo = weakAndImmune match { case reImmuneTo(l) => l.split(", ").toList; case _ => List[String]() }
        idx+startIdx -> Group(team, num.toInt, hp.toInt, weakTo, immuneTo, attack.toInt, attackType, initiative.toInt)
    }.toMap
  }
}
