package adventofcode.solutions

import adventofcode.Day

object Day07 extends Day(7) {

  sealed abstract class Mode
  case object Running extends Mode
  case object Terminated extends Mode
  case object Halted extends Mode

  case class State(program: IndexedSeq[Int], pointer: Int = 0, input: Seq[Int], output: Seq[Int] = Seq.empty)

  val program = input.split(",").map(_.toInt)

  def step(state: State): (Mode, State) = {
    val State(program, pointer, input, output) = state

    def off(i: Int): Int = pointer + i
    def immediate(i: Int): Int = program(off(i))
    def position(i: Int): Int = program(immediate(i))

    val (opcode, modes) = {
      val modeStr = immediate(0).toString.reverse.padTo(5, '0')
      val (ed, cba) = modeStr.splitAt(2)
      (ed.reverse.toInt, cba.toSeq.map(_ != '0'))
    }

    def p(i: Int): Int = if(modes(i - 1)) immediate(i) else position(i)

    def toInt(b: Boolean): Int = if(b) 1 else 0
    def binary(f: (Int, Int) => Int): State = state.copy(program.updated(immediate(3), f(p(1), p(2))), off(4))
    def branchIf(f: Int => Boolean): State = state.copy(pointer = if(f(p(1))) p(2) else off(3))
    def storeIf(f: (Int, Int) => Boolean): State = state.copy(program.updated(immediate(3), toInt(f(p(1), p(2)))), off(4))

    opcode match {
      case 1 => (Running, binary(_ + _))
      case 2 => (Running, binary(_ * _))
      case 3 =>
        input match {
          case head +: tail => (Running, state.copy(program.updated(immediate(1), head), off(2), tail))
          case Seq() => (Halted, state)
        }
      case 4 => (Running, state.copy(pointer = off(2), output = output :+ p(1)))
      case 5 => (Running, branchIf(_ != 0))
      case 6 => (Running, branchIf(_ == 0))
      case 7 => (Running, storeIf(_ < _))
      case 8 => (Running, storeIf(_ == _))
      case 99 => (Terminated, state)
    }
  }

  def execute(state: State): (Mode, State) = {
    val (mode, next) = step(state)
    if(mode == Running) {
      execute(next)
    } else {
      (mode, next)
    }
  }

  def chained(phases: Seq[Int]): Int = phases.foldLeft(0)((a, e) => execute(State(program, input = Seq(e, a)))._2.output.head)

  override def solutionA = (0 to 4).permutations.map(chained).max


  def feebackLoop(states: IndexedSeq[(Mode, State)], i: Int = 0): Int = {
    val (mode, state) = states(i)
    val (pi, ni) = ((i + states.size - 1) % states.size, (i + 1) % states.size)
    val isLast = i == states.size - 1

    val previous = states(pi)
    if(mode == Terminated) {
      if(isLast) {
        state.output.head
      } else {
        feebackLoop(states, ni)
      }
    } else {
      val next = execute(state.copy(input = state.input ++ previous._2.output))
      val nextStates = states.updated(pi, previous.copy(_2 = previous._2.copy(output = Seq.empty))).updated(i, next)
      feebackLoop(nextStates, ni)
    }
  }

  def looped(phases: IndexedSeq[Int]): Int = feebackLoop((Seq(phases.head, 0) +: phases.tail.map(v => Seq(v))).map(in => (Running, State(program, input = in))))

  override def solutionB = (5 to 9).permutations.map(looped).max

}
