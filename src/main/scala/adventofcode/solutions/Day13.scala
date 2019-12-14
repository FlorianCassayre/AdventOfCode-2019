package adventofcode.solutions

import adventofcode.Day

object Day13 extends Day(13) {

  sealed abstract class Tile
  case object Empty extends Tile
  case object Wall extends Tile
  case object Block extends Tile
  case object Paddle extends Tile
  case object Ball extends Tile

  case class Vec(x: Int, y: Int)
  case class State(program: Map[Int, Int], pointer: Int = 0, base: Int = 0, stepMode: Boolean = false, output: Seq[Int] = Seq.empty, map: Map[Vec, Tile] = Map.empty, score: Int = 0)

  val program = input.split(",").map(_.toInt).zipWithIndex.map(_.swap).toMap.withDefaultValue(0)

  def execute(state: State): (Map[Vec, Tile], Int) = {
    val State(program, pointer, base, stepMode, output, map, score) = state

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
        if(stepMode) {
          (map, score)
        } else {
          val paddle = map.find(_._2 == Paddle).get._1
          val nextBall = execute(state.copy(program + (w(1) -> 0), off(2), stepMode = true))._1.find(_._2 == Ball).get._1
          val offset = nextBall.x - paddle.x - 1
          val nextInput: Int = if(offset > 0) 1 else if(offset < 0) -1 else 0

          execute(state.copy(program + (w(1) -> nextInput), off(2)))
        }
      case 4 =>
        val (nextOutput, nextMap, nextScore) = output :+ p(1) match {
          case Seq(-1, 0, newScore: Int) => (Seq.empty, map, newScore)
          case Seq(x, y, id) =>
            val tile = id match {
              case 0 => Empty
              case 1 => Wall
              case 2 => Block
              case 3 => Paddle
              case 4 => Ball
            }
            val pixel = Vec(x, y)
            (Seq.empty, map + (pixel -> tile), score)
          case other => (other, map, score)
        }
        execute(state.copy(pointer = off(2), output = nextOutput, map = nextMap, score = nextScore))
      case 5 => execute(branchIf(_ != 0))
      case 6 => execute(branchIf(_ == 0))
      case 7 => execute(storeIf(_ < _))
      case 8 => execute(storeIf(_ == _))
      case 9 => execute(state.copy(base = base + p(1), pointer = off(2)))
      case 99 => (map, score)
    }
  }

  override def solutionA = execute(State(program))._1.values.count(_ == Block)

  override def solutionB = execute(State(program + (0 -> 2)))._2

}
