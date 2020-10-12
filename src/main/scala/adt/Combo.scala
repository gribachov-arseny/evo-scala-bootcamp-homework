package adt

import Interfaces._

    /* Unfinished part */

sealed abstract class Combo(inner: CardGroup, outer: CardGroup) extends Valuable[Combo]{

  override def evaluate: Int = {
      val x = Combo.step
      val a = Combo.evaluate(this)
      val b = this.inner.evaluate
      val c = this.outer.evaluate

      a * (x*x) + b*x + c
  }

  }

object Combo {
  val step = 1000000  // <- Big enough value to avoid overlapping of inner & outer eval.values

  def evaluate: Combo => Int = ???

  //Smart Constructors
  def getStraightFlush:      CardGroup => Combo = ???
  def getFourCardsOfOneKind: CardGroup => Combo = ???
  def FullHouse:             CardGroup => Combo = ???
  def getFlush:              CardGroup => Combo = ???
  def getStraight:           CardGroup => Combo = ???
  def getTriple:             CardGroup => Combo = ???
  def getTwoPairs:           CardGroup => Combo = ???
  def getPair:               CardGroup => Combo = ???
  def getTrivial:            CardGroup => Combo = ???

  // Main Smart Constructor
  def getBestCombo: CardGroup => Combo = {
    case cs if Check.tryStraightFlush(cs).isDefined      => getStraightFlush(cs)
    case cs if Check.tryFourCardsOfOneKind(cs).isDefined => getFourCardsOfOneKind(cs)
    case cs if Check.tryFullHouse(cs).isDefined          => FullHouse(cs)
    case cs if Check.tryFlush(cs).isDefined              => getFlush(cs)
    case cs if Check.tryStraight(cs).isDefined           => getStraight(cs)
    case cs if Check.tryTriple(cs).isDefined             => getTriple(cs)
    case cs if Check.tryTwoPairs(cs).isDefined           => getTwoPairs(cs)
    case cs if Check.tryPair(cs).isDefined               => getPair(cs)
    case cs                                              => getTrivial(cs)
  }

}

  /* Scope for functions that are related to searching of Combos */

object Check {

  // functions for export
  val tryStraightFlush: CardGroup => Option[CardGroup] = ???
  val tryFourCardsOfOneKind: CardGroup => Option[CardGroup] = ???
  val tryFullHouse: CardGroup => Option[CardGroup] = ???
  val tryFlush: CardGroup => Option[CardGroup] = ???
  val tryStraight: CardGroup => Option[CardGroup] = ???
  val tryTriple: CardGroup => Option[CardGroup] = ???
  val tryTwoPairs: CardGroup => Option[CardGroup] = ???
  val tryPair: CardGroup => Option[CardGroup] = ???

  //inner functions

  // . . .

}


















