package day22

import scala.collection.mutable

// This is the python algorithm from https://www.redblobgames.com/pathfinding/a-star/implementation.html
// just translated to Scala. Definitely not good idiomatic scala.
object AStar {

  def apply[State](start: State,
                   goal: State,
                   neighbors: State => List[State],
                   heuristic: (State,State) => Long,
                   costFunc: (State,State) => Long)
  : (Map[State, Option[State]], Map[State, Long]) =
  {
    implicit val ordering: Ordering[(State, Long)] = Ordering.by[(State, Long), Long](-1 * _._2)
    val frontier = mutable.PriorityQueue[(State, Long)]((start, 0))

    val cameFrom = mutable.Map[State, Option[State]](start -> None)
    val costSoFar = mutable.Map[State, Long](start -> 0)

    while (frontier.nonEmpty) {
      val (current, _) = frontier.dequeue()

      if (current == goal) {
        return (cameFrom.toMap, costSoFar.toMap)
      }

      for(next <- neighbors(current)) {
        val newCost = costSoFar(current) + costFunc(current, next)
        if (costSoFar.get(next).forall(newCost < _)) {
          costSoFar.update(next, newCost)
          val priority = newCost + heuristic(next, goal)
          frontier.+=((next, priority))
          cameFrom.update(next, Some(current))
        }
      }
    }
    (cameFrom.toMap, costSoFar.toMap)
  }

}