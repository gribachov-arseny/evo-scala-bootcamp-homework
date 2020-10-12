package adt

import Interfaces._

abstract case class Nominal private (value: Int) extends Element[Nominal]

object Nominal {

  // Smart Constructor
  def create(value: Int): Option[Nominal] = {
    if (value < 1 || 13 < value) None
    else Some(new Nominal(value) {
      override val parse: String => Option[Nominal] =
        str => if (str.tail != "") Nominal.parse(str.head)
        else None

      override def write: String = Nominal.write(this)
      override def toString: String = this.write

      override def evaluate: Int = value
    })
  }

  // Parser
  def parse: Char => Option[Nominal] = {
    case '1'            => None
    case x if x.isDigit => create(x.toInt)
    case 'T'            => create(10)
    case 'J'            => create(11)
    case 'Q'            => create(12)
    case 'K'            => create(13)
    case 'A'            => create(14)
    case _              => None
  }

  //Writer
  def write[A <: Nominal]: A => String = {
    case a if a.value == 14 => "A"
    case a if a.value == 10 => "T"
    case a if a.value == 11 => "J"
    case a if a.value == 12 => "Q"
    case a if a.value == 13 => "K"
    case a                  => a.toString
  }

}