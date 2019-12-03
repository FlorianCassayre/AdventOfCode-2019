package adventofcode.solutions

import adventofcode.Day

object Day03 extends Day(3) {

  case class Vec(x: Int, y: Int) {
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y)
    def *(n: Int): Vec = Vec(x * n, y * n)
    def manhattan(that: Vec): Int = Math.abs(x - that.x) + Math.abs(y - that.y)
  }
  case class Wire(direction: Vec, length: Int)

  val directions = Map(
    'U' -> Vec(0, -1),
    'D' -> Vec(0, 1),
    'L' -> Vec(-1, 0),
    'R' -> Vec(1, 0)
  )

  val wires = lines.map(_.split(",").map(s => Wire(directions(s.head), s.tail.toInt)).toSeq)

  val center = Vec(0, 0)

  def unroll(wire: Seq[Wire]): Map[Vec, Int] = wire.foldLeft((center -> 0, Seq.empty[(Vec, Int)])) {
    case ((point -> steps, set), w) =>
      val path = (1 to w.length).map(i => (point + w.direction * i, steps + i))
      (path.last, set ++ path)
  }._2.groupMapReduce(_._1)(_._2)(Math.min)

  val points = wires.map(unroll)

  override def solutionA = points.map(_.keySet).reduce(_ intersect _).map(center.manhattan).min

  val Seq(as: Map[Vec, Int], bs) = points

  override def solutionB = (as.toSeq ++ bs).groupMap(_._1)(_._2).values.filter(_.tail.nonEmpty).map(_.sum).min

}
