package adventofcode.solutions

import adventofcode.Day

object Day06 extends Day(6) {

  val orbits = lines.map {
    case s"$a)$b" => (a, b)
  }

  val nodes = orbits.flatMap{ case (a, b) => Seq(a, b) }.toSet

  def adjacency(set: Seq[(String, String)]): Map[String, Set[String]] = set.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap

  val directed = adjacency(orbits)

  def closure(id: String): Int = directed.get(id).toSeq.flatten.map(to => 1 + closure(to)).sum

  override def solutionA = nodes.toSeq.map(closure).sum

  val undirected = adjacency(orbits ++ orbits.map(_.swap))

  val (you, santa) = ("YOU", "SAN")

  def bfs(current: Set[String], visited: Set[String] = Set.empty, hops: Int = -2): Int = {
    if(current.contains(santa)) {
      hops
    } else {
      val nextVisited = current ++ visited
      val next = current.flatMap(undirected.get).flatten.diff(nextVisited)
      bfs(next, nextVisited, hops + 1)
    }
  }

  override def solutionB = bfs(Set(you))

}
