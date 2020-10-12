package adt

import Interfaces._

  /* Suit */

sealed trait Suit extends Element[Suit] {
    override def evaluate: Int = Suit.order.indexOf(this)

    // input: 1 Char inside String container
    override val parse: String => Option[Suit] =
      str => if (!str.tail.isEmpty) None
      else Suit.parse(str.head)

    def write: String = Suit.write(this).toString

    override def toString: String = Suit.show(this).toString
  }

object Suit {
    val order: List[Suit] = List(Club,Spade,Diamond,Heart)

    final case object Club extends Suit
    final case object Spade extends Suit
    final case object Diamond extends Suit
    final case object Heart extends Suit

    val parse: Char => Option[Suit] = {
      case 'c' => Some(Club)
      case 's' => Some(Spade)
      case 'd' => Some(Diamond)
      case 'h' => Some(Heart)
      case _   => None
    }

    val write: Suit => Char = {
      case Club     =>    'c'
      case Spade    =>    's'
      case Diamond  =>    'd'
      case Heart    =>    'h'
    }

    def show: Suit => Char = {
      case Club     =>    '♣'
      case Spade    =>    '♠'
      case Diamond  =>    '♦'
      case Heart    =>    '♥'
    }

}


