package adventofcode.solutions

import adventofcode.Day

object Day04 extends Day(4) {

  val range = input match {
    case s"$a-$b" => a.toInt to b.toInt
  }

  val ascending = range.map(_.toString.toSeq).filter(s => s == s.sorted)

  val running = ascending.map(LazyList.unfold(_)(s => s.headOption.map(h =>
    s.tail.span(_ == h)
  ).map(t => t.copy(_1 = t._1.size + 1))))

  override def solutionA = running.count(_.exists(_ >= 2))

  override def solutionB = running.count(_.exists(_ == 2))

}
