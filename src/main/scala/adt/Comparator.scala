package adt

import Interfaces._

    /* Representation of one iteration of the program */

sealed trait Comparator extends ReadWritable[Comparator] {
  val board: CardGroup  //Board
  val hands: CardGroup  //List[Hand]

  override val parse: String => Option[Comparator] = Comparator.parse
  def sorted: Comparator = Comparator.sort(this)
  override def write: String = Comparator.write(this)

  def run: String => Option[String] =
    str => for (x <- parse(str))
           yield x.sorted.write

}

object Comparator {

  def write: Comparator => String = ???

  def sort: Comparator => Comparator = ??? // returns new comparator with sorted field hands

  def parse: String => Option[Comparator] = ???

}
