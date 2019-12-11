package adventofcode.solutions

import adventofcode.Day

object Day11 extends Day(11) {

  case class Vec(x: Int, y: Int) {
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y)
    def clockwise: Vec = Vec(-y, x)
    def counterClockwise: Vec = Vec(y, -x)
  }

  case class State(program: Map[BigInt, BigInt], pointer: BigInt = 0, base: BigInt = 0, output: Option[BigInt] = None,
                   location: Vec = Vec(0, 0), direction: Vec = Vec(0, -1), map: Map[Vec, Boolean] = Map.empty)

  val program = input.split(",").map(BigInt.apply).zipWithIndex.map(_.swap).map(t => t.copy(_1 = BigInt(t._1))).toMap.withDefaultValue(BigInt(0))

  def execute(state: State): Map[Vec, Boolean] = {
    val State(program, pointer, base, output, location, direction, map) = state

    def off(i: BigInt): BigInt = pointer + i
    def immediate(i: BigInt): BigInt = program(off(i))
    def position(i: BigInt): BigInt = program(immediate(i))
    def relative(i: BigInt): BigInt = program(immediate(i) + base)

    val (opcode, modes) = {
      val modeStr = immediate(0).toString.reverse.padTo(5, '0')
      val (ed, cba) = modeStr.splitAt(2)
      (ed.reverse.toInt, cba.toSeq.map(_.asDigit))
    }

    def p(i: Int): BigInt = modes(i - 1) match {
      case 0 => position(i)
      case 1 => immediate(i)
      case 2 => relative(i)
    }

    def w(i: Int): BigInt = modes(i - 1) match {
      case 0 => immediate(i)
      case 2 => immediate(i) + base
    }

    def toInt(b: Boolean): Int = if(b) 1 else 0
    def binary(f: (BigInt, BigInt) => BigInt): State = state.copy(program + (w(3) -> f(p(1), p(2))), off(4))
    def branchIf(f: BigInt => Boolean): State = state.copy(pointer = if(f(p(1))) p(2) else off(3))
    def storeIf(f: (BigInt, BigInt) => Boolean): State = state.copy(program + (w(3) -> toInt(f(p(1), p(2)))), off(4))

    val (zero, one) = (BigInt(0), BigInt(1))
    def input: BigInt = if(map.getOrElse(location, false)) one else zero
    def toBoolean(v: BigInt): Boolean = v match {
      case `zero` => false
      case `one` => true
    }

    opcode match {
      case 1 => execute(binary(_ + _))
      case 2 => execute(binary(_ * _))
      case 3 => execute(state.copy(program + (w(1) -> input), off(2)))
      case 4 =>
        val nextState = state.copy(pointer = off(2))
        output match {
          case Some(color) =>
            val mapUpdated = map + (location -> toBoolean(color))
            val newDirection = if(toBoolean(p(1))) direction.clockwise else direction.counterClockwise
            execute(nextState.copy(output = None, location = location + newDirection, direction = newDirection, map = mapUpdated))
          case None => execute(nextState.copy(output = Some(p(1))))
        }
      case 5 => execute(branchIf(_ != 0))
      case 6 => execute(branchIf(_ == 0))
      case 7 => execute(storeIf(_ < _))
      case 8 => execute(storeIf(_ == _))
      case 9 => execute(state.copy(base = base + p(1), pointer = off(2)))
      case 99 => map
    }
  }

  override def solutionA = execute(State(program)).size

  val resultSet = execute(State(program, map = Map(Vec(0, 0) -> true))).toSet.filter(_._2).map(_._1)
  val result = resultSet.toSeq

  def range(seq: Seq[Int]): Range = seq.min to seq.max
  val (rangeX, rangeY) = (range(result.map(_.x)), range(result.map(_.y)))

  override def solutionB = rangeY.map(y => rangeX.map(x => if(resultSet.contains(Vec(x, y))) "#" else " ").mkString).mkString(lineSeparator)

}
