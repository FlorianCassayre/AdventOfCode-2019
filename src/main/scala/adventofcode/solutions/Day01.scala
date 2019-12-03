package adventofcode.solutions

import adventofcode.Day

object Day01 extends Day(1) {

  val masses = lines.map(_.toInt)

  def fuel(mass: Int): Int = mass / 3 - 2

  override def solutionA = masses.map(fuel).sum

  override def solutionB = masses.flatMap(LazyList.iterate(_)(fuel).takeWhile(_ > 0).tail).sum

}
