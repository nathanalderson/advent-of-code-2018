/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package day14

// ugh, custom mutable data structure
case class Recipe(data: Int) {
  var next: Recipe = this
  var prev: Recipe = this

  def insertAfter(a: Int): Recipe = {
    val n = Recipe(a)
    n.next = this.next
    n.prev = this
    this.next = n
    n.next.prev = n
    n
  }

  def toRight(i: Int): Recipe =
    (0 until i).foldLeft(this){ case (r,_) => r.next }

  def toLeft(i: Int): Recipe =
    (0 until i).foldLeft(this){ case (r,_) => r.prev }
}

object State {
  def initialState: State = {
    val recipe1 = Recipe(3)
    val recipe2 = recipe1.insertAfter(7)
    State(recipe1, recipe2, recipe1, recipe2, 2)
  }
}
case class State(elf1: Recipe, elf2: Recipe, beginning: Recipe, end: Recipe, size: Int) {
  def append(r: Int): State =
    State(elf1, elf2, beginning, end.insertAfter(r), size + 1)

  def step: State =
    (elf1.data + elf2.data)
      .toString
      .map(_.asDigit)
      .foldLeft(this) { case (s, r) => s.append(r) } // append new recipes
      .copy(elf1 = elf1.toRight(elf1.data + 1), elf2 = elf2.toRight(elf2.data + 1))

  def nAfterM(n: Int, m: Int): List[Int] = {
    assert(n+m <= size)
    Iterator.iterate(end.toLeft(size - m - 1))(_.toRight(1))
      .take(n)
      .map(_.data)
      .toList
  }

  def lastN(n: Int): List[Int] = {
    Iterator.iterate(end)(_.toLeft(1))
      .take(n)
      .map(_.data)
      .toList
      .reverse
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(s"ans1 = ${ans1(540391)._2}")
    println(s"ans2 = ${ans2("540391")}")
  }

  def steps: Iterator[State] = Iterator.iterate(State.initialState)(_.step)

  def ans1(numRecipes: Int): (State, String) = {
    val finalState = steps
      .dropWhile(_.size < numRecipes+10)
      .next
    val next10 = finalState.nAfterM(10, numRecipes).mkString
    (finalState, next10)
  }

  def ans2(scoresStr: String): Int = {
    val scores = scoresStr.map(_.toString.toInt)
    steps.map { state =>
      val lastN = state.lastN(scores.length+1)
      if (lastN.startsWith(scores))
        (state, Some(state.size - scores.length - 1))
      else if (lastN.endsWith(scores))
        (state, Some(state.size - scores.length))
      else
        (state, None)
    }.dropWhile(_._2.isEmpty).next._2.get
  }
}
