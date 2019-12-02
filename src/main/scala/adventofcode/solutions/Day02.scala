package adventofcode.solutions

import adventofcode.Day

object Day02 extends Day(2) {

  val program = input.split(",").map(_.toInt)

  def executeWithInitials(a: Int, b: Int): Int = {
    def execute(program: IndexedSeq[Int], pointer: Int = 0): Int = {
      def address(i: Int): Int = program(pointer + i)
      def load(i: Int): Int = program(address(i))
      val next = pointer + 4
      def apply(f: (Int, Int) => Int): IndexedSeq[Int] = program.updated(address(3), f(load(1), load(2)))
      program(pointer) match {
        case 1 => execute(apply(_ + _), next)
        case 2 => execute(apply(_ * _), next)
        case 99 => program.head
      }
    }

    execute(program.updated(1, a).updated(2, b))
  }

  override def solutionA = executeWithInitials(12, 2)

  val range = 0 to 99
  val magic = 19690720

  override def solutionB = range.flatMap(noun => range.find(verb => executeWithInitials(noun, verb) == magic).map(verb => 100 * noun + verb)).head

}
