package adventofcode.solutions

import adventofcode.Day

object Day08 extends Day(8) {

  val (width, height) = (25, 6)

  val image = input.map(_.asDigit).grouped(width).toIndexedSeq.grouped(height).toIndexedSeq

  val layer = image.map(_.flatten).minBy(_.count(_ == 0))

  override def solutionA = layer.count(_ == 1) * layer.count(_ == 2)

  def at(y: Int)(x: Int): Int = image.map(_(y)(x)).find(_ != 2).get

  val blend = (0 until height).map(y => (0 until width).map(x => at(y)(x)))

  override def solutionB = blend.map(_.map(_ * 3 + ' ').map(_.toChar).mkString).mkString(lineSeparator)

}
