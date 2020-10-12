package adt

import Interfaces._

sealed abstract case class Card private (n: Nominal, s: Suit) extends ReadWritable[Card] {
  val nominal: Nominal
  val suit: Suit
}

object Card {

  def create(n: Nominal, s: Suit): Card =
    new Card(n,s) {
      override val nominal: Nominal = n
      override val suit: Suit = s

      override val parse: String => Option[Card] =
        str => str.toList match {
          case a :: b :: Nil => Card.parse(a)(b)
          case _             => None
        }

      override def write: String = Card.write(this)
    }

  def parse: Char => Char => Option[Card] = nominal => suit => for {
    n <- Nominal.parse(nominal)
    s <- Suit.parse(suit)
  } yield create(n,s)

  def write(c: Card): String = c.nominal.write + c.suit.write
}

                /* Ordering Wrappers */

class NominalOrdering(val card: Card) extends AnyVal with Valuable[NominalOrdering] {
  override def evaluate: Int = this.card.nominal.evaluate
}

class SuitOrdering(val card: Card) extends AnyVal with Valuable[SuitOrdering] {
  override def evaluate: Int = this.card.suit.evaluate
}



