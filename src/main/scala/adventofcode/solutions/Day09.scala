package adventofcode.solutions

import adventofcode.Day

object Day09 extends Day(9) {

  case class State(program: Map[BigInt, BigInt], pointer: BigInt = 0, base: BigInt = 0, output: Seq[BigInt] = Seq.empty)

  val program = input.split(",").map(BigInt.apply).zipWithIndex.map(_.swap).map(t => t.copy(_1 = BigInt(t._1))).toMap.withDefaultValue(BigInt(0))

  def executeWithInput(program: Map[BigInt, BigInt], input: Int): Seq[BigInt] = {
    def execute(state: State): Seq[BigInt] = {
      val State(program, pointer, base, output) = state

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

      opcode match {
        case 1 => execute(binary(_ + _))
        case 2 => execute(binary(_ * _))
        case 3 => execute(state.copy(program + (w(1) -> input), off(2)))
        case 4 => execute(state.copy(pointer = off(2), output = output :+ p(1)))
        case 5 => execute(branchIf(_ != 0))
        case 6 => execute(branchIf(_ == 0))
        case 7 => execute(storeIf(_ < _))
        case 8 => execute(storeIf(_ == _))
        case 9 => execute(state.copy(base = base + p(1), pointer = off(2)))
        case 99 => output
      }
    }

    execute(State(program))
  }

  override def solutionA = executeWithInput(program, 1).head

  override def solutionB = executeWithInput(program, 2).head

}
