package day22

import scala.collection.mutable

object PathFinder {

  def apply[T](start:T, goal:T, neighbors:T => List[T], heuristic:(T,T)=>Float, costFunc:(T,T)=>Float):List[T] = {

    def calculateCost(path:(Float, List[T])) =
      path._2.tail.foldLeft((0f,path._2.head))((costHead,node) => {
          val (cost, head) = costHead
          (cost + costFunc(node,head), node)
        })._1

    def findBestCandidate(candidates:mutable.Set[(Float, List[T])]) =
      candidates.foldLeft(
        (Float.PositiveInfinity, (0f, List[T]())))((best, path) => {
          val guesstimate = path._1 + heuristic(path._2.head, goal)
          if (best._1 <  guesstimate) best else (guesstimate,path)
        })._2

    // Uses mutable set for performance... ?
    var fringe = mutable.Set((0f,start::Nil))
    var closed = mutable.Set[T]()

    while(fringe.nonEmpty) {
      val (cost, current) = findBestCandidate(fringe)
      fringe = fringe - ((cost, current))
      if(current.head equals goal)
        return current.reverse
      else if (!closed.contains(current.head )) {
        closed = closed + current.head
        neighbors(current.head).foreach(n => {
            val nCost = costFunc(current.head, n) + cost
            fringe = fringe + ((nCost, n::current))
          })
      }
    }
    // Searched all possible paths...
    Nil
  }
}