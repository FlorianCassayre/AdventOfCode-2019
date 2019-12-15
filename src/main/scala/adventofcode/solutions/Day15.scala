package adventofcode.solutions

import adventofcode.Day

object Day15 extends Day(15) {

  case class Vec(x: Int, y: Int) {
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y)
  }

  val Zero = Vec(0, 0)

  val directions = Seq(Vec(0, -1), Vec(0, 1), Vec(-1, 0), Vec(1, 0))

  case class State(program: Map[Int, Int], pointer: Int = 0, base: Int = 0, lastLocation: Vec = Zero, location: Vec = Zero, goal: Option[Vec] = None, map: Map[Vec, Boolean] = Map(Zero -> false))

  val program = input.split(",").map(_.toInt).zipWithIndex.map(_.swap).toMap.withDefaultValue(0)

  def execute(state: State): (Map[Vec, Boolean], Vec) = {
    val State(program, pointer, base, lastLocation, location, goal, map) = state

    def off(i: Int): Int = pointer + i
    def immediate(i: Int): Int = program(off(i))
    def position(i: Int): Int = program(immediate(i))
    def relative(i: Int): Int = program(immediate(i) + base)

    val (opcode, modes) = {
      val modeStr = immediate(0).toString.reverse.padTo(5, '0')
      val (ed, cba) = modeStr.splitAt(2)
      (ed.reverse.toInt, cba.toSeq.map(_.asDigit))
    }

    def p(i: Int): Int = modes(i - 1) match {
      case 0 => position(i)
      case 1 => immediate(i)
      case 2 => relative(i)
    }

    def w(i: Int): Int = modes(i - 1) match {
      case 0 => immediate(i)
      case 2 => immediate(i) + base
    }

    def toInt(b: Boolean): Int = if(b) 1 else 0
    def binary(f: (Int, Int) => Int): State = state.copy(program + (w(3) -> f(p(1), p(2))), off(4))
    def branchIf(f: Int => Boolean): State = state.copy(pointer = if(f(p(1))) p(2) else off(3))
    def storeIf(f: (Int, Int) => Boolean): State = state.copy(program + (w(3) -> toInt(f(p(1), p(2)))), off(4))

    opcode match {
      case 1 => execute(binary(_ + _))
      case 2 => execute(binary(_ * _))
      case 3 =>
        def dfs(current: Vec, visited: Set[Vec] = Set.empty): Option[Vec] = {
          val newVisited = visited + current
          val possible = directions.filter(d => !visited.contains(current + d)).filter(d => !map.get(current + d).contains(true))
          possible.find(d => map.get(current + d).isEmpty).map(Some.apply)
            .getOrElse(possible.foldLeft[Option[Vec]](None)((acc, d) => acc.orElse(dfs(current + d, newVisited).map(_ => d))))
        }

        dfs(location) match {
          case Some(dir) => execute(state.copy(program + (w(1) -> (directions.indexOf(dir) + 1)), off(2), lastLocation = location, location = location + dir))
          case None => (map, goal.get)
        }
      case 4 =>
        val s = state.copy(pointer = off(2))
        p(1) match {
          case 0 => execute(s.copy(location = lastLocation, map = map + (location -> true)))
          case 1 => execute(s.copy(map = map + (location -> false)))
          case 2 => execute(s.copy(goal = Some(location), map = map + (location -> false)))
        }
      case 5 => execute(branchIf(_ != 0))
      case 6 => execute(branchIf(_ == 0))
      case 7 => execute(storeIf(_ < _))
      case 8 => execute(storeIf(_ == _))
      case 9 => execute(state.copy(base = base + p(1), pointer = off(2)))
    }
  }

  def bfs(current: Set[Vec], condition: Set[Vec] => Boolean, visited: Set[Vec] = Set.empty, distance: Int = 0): Int = {
    if(condition(current)) {
      distance
    } else {
      val reachable = current.flatMap(v => directions.map(_ + v).filterNot(map)).diff(visited)
      bfs(reachable, condition, visited ++ current, distance + 1)
    }
  }

  val (map, goal) = execute(State(program))

  override def solutionA = bfs(Set(Zero), _.contains(goal))

  override def solutionB = bfs(Set(goal), _.isEmpty) - 1

}
