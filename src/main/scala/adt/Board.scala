package adt

//type Board = CardGroup  // <- is it prohibited to make trivial alias? (without Type substitution)

object Board {

  def createBoard: List[Card] => Option[CardGroup] = CardGroup.create(5)(_)

}



  /* Old try */



/*
class Board(val cs: CardGroup) extends AnyVal

object Board {

  def createBoard(n: Int)(ls: List[Card]): Option[Board] =
    for (x <- CardGroup.create(n)(ls))
      yield new Board(x)

  def createNormalBoard: List[Card] => Option[Board] = createBoard(5)(_)

  /* Evaluation */

  def evaluateHand: Board => Int = ???

}
*/