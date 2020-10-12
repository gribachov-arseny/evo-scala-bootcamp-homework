package adt

object Hand {

  def createTexasHand: List[Card] => Option[CardGroup] = CardGroup.create(2)(_)
  def createOmahaHand: List[Card] => Option[CardGroup] = CardGroup.create(4)(_)

}






          /* Old */

/*
class Hand(val toCardGroup: CardGroup) extends AnyVal with Element[Hand] with Monoid[Hand] {

      override val mempty: Hand = new Hand(this.toCardGroup.mempty)
      override def +(x: Hand, y: Hand): Hand = new Hand(CardGroup.concat(x.toCardGroup,y.toCardGroup))

      override val parse: String => Option[Hand] =
        str => for (x <- CardGroup.parse(str))
                   yield new Hand(x)

      override def write: String = this.toCardGroup.write

      override def evaluate: Int = ???
    }
*/