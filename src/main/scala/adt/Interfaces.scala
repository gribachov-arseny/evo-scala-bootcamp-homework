package adt

object Interfaces {

            /* Simple Interfaces */

  trait Valuable[TSelf <: Valuable[TSelf]] extends Ordered[TSelf] {
    def evaluate: Int
    override def compare(that: TSelf): Int = this.evaluate - that.evaluate
  }

  trait ReadWritable[TSelf] {
    val parse: String => Option[TSelf]      // <- can be treated as a Constructor
    def write: String
  }

  trait Element[TSelf <: Element[TSelf]] extends Valuable[TSelf] with ReadWritable[TSelf]


  // for easy CardGroup concatenation. It'll be used in combo-searching (Hand + Board)    // <- usage has not been implemented
  trait Monoid[M <: Monoid[M]] {
    val mempty: M
    def +(x: M, y: M): M     // <- this option works

    //def +[M]: M => M => M     <- this option does not work.
    //                             in case of instantiation Ms are not substituted
    //                             Unclear, why?
  }

}
