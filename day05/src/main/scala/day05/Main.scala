/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package day05

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.mkString
    val output = react(input)
    println(s"ans1 = ${output.length}")
    val (_, minimal) = ans2(input)
    println(s"ans2 = ${minimal.length}")
  }

  def ans2(in: String): (Char, String) =
    ('a' to 'z')
      .map(c => c->in.filterNot(_.toLower==c))
      .toMap
      .mapValues(react)
      .minBy(_._2.length)

  def react(in: String): String = react(in.toList).mkString
  def react(in: Iterable[Char]): Iterable[Char] = in match {
    case List() => List()
    case List(c) => List(c)
    case List(c1,c2) => reactSingle(c1,c2)
    case l =>
      val pass1 = l.sliding(2,2).flatMap(react).toList
      val pass2 = if (pass1.length > 2)
        pass1.head :: pass1.tail.sliding(2,2).flatMap(react).toList
      else
        pass1
      if (pass2 == in) pass2 else react(pass2)
  }

  def reactSingle(c1: Char, c2: Char): List[Char] =
    if (c1.toLower == c2.toLower && c1 != c2) List() else List(c1,c2)
}
