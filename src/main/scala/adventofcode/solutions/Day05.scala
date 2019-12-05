package adventofcode.solutions

import adventofcode.Day

object Day05 extends Day(5) {

  val program = input.split(",").map(_.toInt)

  case class State(program: IndexedSeq[Int], pointer: Int = 0, output: Seq[Int] = Seq.empty)

  def executeWithInput(program: IndexedSeq[Int], input: Int): Seq[Int] = {
    def execute(state: State): Seq[Int] = {
      val State(program, pointer, output) = state

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
        case 1 => execute(binary(_ + _))
        case 2 => execute(binary(_ * _))
        case 3 => execute(state.copy(program.updated(immediate(1), input), off(2)))
        case 4 => execute(state.copy(pointer = off(2), output = output :+ p(1)))
        case 5 => execute(branchIf(_ != 0))
        case 6 => execute(branchIf(_ == 0))
        case 7 => execute(storeIf(_ < _))
        case 8 => execute(storeIf(_ == _))
        case 99 => output
      }
    }

    execute(State(program))
  }

  override def solutionA = executeWithInput(program, 1).last

  override def solutionB = executeWithInput(program, 5).head

}
