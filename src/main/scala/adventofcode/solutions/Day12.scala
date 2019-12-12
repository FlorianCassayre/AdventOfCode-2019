package adventofcode.solutions

import adventofcode.Day

object Day12 extends Day(12) {

  case class Vec(x: Int, y: Int, z: Int) {
    def coordinates: Seq[Int] = Seq(x, y, z)
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)
    private def g(a: Int, b: Int): Int = if(a < b) 1 else if(a > b) -1 else 0
    def gravity(that: Vec): Vec = Vec(g(x, that.x), g(y, that.y), g(z, that.z))
    def manhattanNorm: Int = x.abs + y.abs + z.abs
  }
  val Zero = Vec(0, 0, 0)
  case class Moon(position: Vec, velocity: Vec)

  val moons = lines.map {
    case s"<x=$x, y=$y, z=$z>" => Vec(x.toInt, y.toInt, z.toInt)
  }.map(Moon(_, Zero))

  def velocities(positions: Seq[Vec]): Seq[Vec] =
    positions.map(a => positions.filter(_ != a).map(b => a.gravity(b)).fold(Zero)(_ + _))

  def next(moons: Seq[Moon]): Seq[Moon] =
    moons.zip(velocities(moons.map(_.position))).map{ case (moon, v) =>
      val dv = moon.velocity + v
      moon.copy(velocity = dv, position = moon.position + dv)
    }

  def iterate(moons: Seq[Moon], i: Int): Seq[Moon] = if(i > 0) iterate(next(moons), i - 1) else moons

  override def solutionA = iterate(moons, 1000).map(m => m.position.manhattanNorm * m.velocity.manhattanNorm).sum

  def gcd(a: BigInt, b: BigInt): BigInt = if(b == 0) a else gcd(b, a % b)
  def lcm(a: BigInt, b: BigInt): BigInt = (a * b) / gcd(a, b)

  val n = 3

  def untilLoop(moons: Seq[Moon], history: Seq[Set[Seq[(Int, Int)]]] = Seq.fill(n)(Set.empty),
                solution: Seq[Option[Int]] = Seq.fill(n)(None), i: Int = 0): Seq[Int] = {
    val state = moons.map(m => m.position.coordinates.zip(m.velocity.coordinates)).transpose
    val updatedHistory = history.zip(state).map(t => t._1 + t._2)
    val updatedSolution = solution.zip(updatedHistory).map{ case (o, h) => o.orElse(Option.when(h.size == i)(i)) }
    if(updatedSolution.forall(_.nonEmpty)) updatedSolution.flatten else untilLoop(next(moons), updatedHistory, updatedSolution, i + 1)
  }

  override def solutionB = untilLoop(moons).map(BigInt.apply).reduce(lcm)

}
