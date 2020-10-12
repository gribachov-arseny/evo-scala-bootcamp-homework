package adt

import Interfaces._

sealed abstract case class CardGroup private (cards: List[Card]) extends Element[CardGroup] with Monoid[CardGroup]

object CardGroup {

  // Smart Constructor
  def create(amount: Int): List[Card] => Option[CardGroup] =
    cards => if (cards.length != amount) None
    else Some (new CardGroup(cards) {
      override val parse: String => Option[CardGroup] = CardGroup.parse
      override def write: String = cards flatMap Card.write toString
      override def evaluate: Int = cards map (new NominalOrdering(_)) max   //<- check type
      override val mempty: CardGroup = zero
      override def +(x: CardGroup, y: CardGroup): CardGroup = concat(x,y)
      //val toCombo: Combo = CardGroup.bestCombo(this)
    })

  // Parser
  def parse: String => Option[CardGroup]  = {

    def parse1: List[Char] => List[Option[Card]] = {
      case (a :: b :: xs) => Card.parse(a)(b) :: parse1(xs)
      case Nil => Nil
    }

    // patch for type-constructor commutation
    def sequence: List[Option[Card]] => Option[List[Card]] = {
      input => { if (input forall {_.isEmpty} ) None
      else Some( for {option <- input if option != None
                      Some(value) = for (content <- option)
                        yield content}
        yield value
      )
      }
    }

    str => if (str.length % 2 != 0) None
    else { val res = parse1(str.toList)
           val list = if (res forall {_.isEmpty} ) None
                      else sequence(res)
      list match {
        case Some(xs) => create(xs.length)(xs)
        case None     => None
      }
    }

  }

  def write(cs:CardGroup): String = (cs.cards flatMap Card.write ).toString

  /* Monoid */

  def concat(xs: CardGroup, ys: CardGroup) : CardGroup = {
    val (x,y) = (xs.cards.length,ys.cards.length)
    create(x+y)(xs.cards:::ys.cards) match {case Some(x) => x}
  }

  val zero: CardGroup = create(0)(Nil) match {case Some(x) => x}

      /* Evaluation */    // <- Unfinished

  def fives: CardGroup => Set[CardGroup] = ??? // returns all samples of 5 elements

  def bestCombo: CardGroup => Combo = Combo.getBestCombo(_)

}


