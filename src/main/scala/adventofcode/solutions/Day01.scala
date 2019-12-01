package adventofcode.solutions

import adventofcode.Day

object Day01 extends Day(1) {

  val masses = lines.map(_.toInt)

  def fuel(mass: Int): Int = mass / 3 - 2

  override def solutionA = masses.map(fuel).sum

  def fuelIt(initial: Int): Int = {
    lazy val stream: LazyList[Int] = initial #:: stream.map(fuel).takeWhile(_ > 0)
    stream.tail.sum
  }

  override def solutionB = masses.map(fuelIt).sum

}
