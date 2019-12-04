package adventofcode.solutions

import adventofcode.Day

object Day04 extends Day(4) {

  val range = input match {
    case s"$a-$b" => a.toInt to b.toInt
  }

  val running = range.map(_.toString.toSeq)
    .filter(s => s == s.sorted)
    .map(_.groupBy(identity).values.map(_.size))

  override def solutionA = running.count(_.exists(_ >= 2))

  override def solutionB = running.count(_.exists(_ == 2))

}
