package adventofcode.solutions

import adventofcode.Day

object Day10 extends Day(10) {

  case class Vec(x: Int, y: Int) {
    private def sq(x: Int): Int = x * x
    private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
    def -(that: Vec): Vec = Vec(x - that.x, y - that.y)
    def normal(that: Vec): Vec = {
      val p = that - this
      val g = gcd(Math.abs(p.x), Math.abs(p.y))
      Vec(p.x / g, p.y / g)
    }
    def distanceSq(that: Vec): Int = sq(x - that.x) + sq(y - that.y)
    def clockDistance(that: Vec): Double = {
      val (a, b) = (Math.atan2(y, x), Math.atan2(that.y, that.x))
      ((b - a) + 2 * Math.PI) % (2 * Math.PI)
    }
  }

  val map = lines.map(_.toSeq.map(_ == '#'))

  val positions =
    for {
      (a, y) <- map.zipWithIndex
      (b, x) <- a.zipWithIndex
      if b
    } yield Vec(x, y)

  val (station, detected) = positions.map(p => p -> positions.filter(_ != p).map(p.normal).distinct.size).maxBy(_._2)

  override def solutionA = detected

  def vaporize(asteroids: Map[Vec, Seq[Vec]], previous: Vec, i: Int): Vec = {
    if(i > 0) {
      val direction = asteroids.keySet.map(q => q -> (previous - station).clockDistance(q)).filter(_._2 != 0).minBy(_._2)._1
      val sight = asteroids(direction)
      val updated = sight.tail.headOption.map(_ => asteroids + (direction -> sight.tail)).getOrElse(asteroids - direction)
      vaporize(updated, sight.head, i - 1)
    } else {
      previous
    }
  }

  val directions = positions.filter(_ != station).groupBy(station.normal).view.mapValues(_.sortBy(station.distanceSq)).toMap

  val last = vaporize(directions, Vec(station.x - 1, station.y - input.length), 200)

  override def solutionB = last.x * 100 + last.y

}
