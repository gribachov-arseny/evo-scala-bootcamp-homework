package main.scala

import scala.io.Source
import scala.util

object ControlStructuresHomework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class ChainFraction(numbers: List[Double]) extends Command        // [a,b,c,d] -> a + 1/(b + 1/(c + 1/d))
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class ChangeMe(value: String) extends Result // adjust Result as required to match requirements
  final case class Data(value: Double) extends Result // adjust Result as required to match requirements

  /* takes input string and converts it into Command or ErrorMessage */
  def parseCommand(input: String): Either[ErrorMessage, Command] = {

    val tokens: List[String] = input split " " filter (_ != "") toList

    def convert(xs: List[String]): List[Double]
    = for (x <- xs if isDouble(x)) yield x.toDouble

    def isDouble: String => Boolean = (str: String) =>
      ((if (str.head == '-') str.tail else str) diff ".") forall (Character.isDigit(_))

    val numbers: List[Double] = convert(tokens.tail)

    if (numbers != Nil)
    tokens match {

      case Nil      => Left(ErrorMessage("Empty input"))

      case "divide"
        :: a :: b :: Nil   => if (isDouble(a) && isDouble(b)) Right(Command.Divide(a.toDouble,b.toDouble))
                              else Left(ErrorMessage("Numbers cannot be parsed"))
      case "divide" :: _   => Left(ErrorMessage("Only two values are allowed to divide"))
      case "sum" :: _      => Right(Command.Sum(numbers))
      case "average" :: _  => Right(Command.Average(numbers))
      case "min" :: _      => Right(Command.Min(numbers))
      case "max" :: _      => Right(Command.Max(numbers))

      case "chain" :: _    => Right(Command.ChainFraction(numbers))
      case _               => Left(ErrorMessage("Command cannot be parsed"))
    } else Left(ErrorMessage("Command should have at least one convertible argument"))

  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
      case Command.Divide( _ , 0 )           => Left(ErrorMessage("Division by zero!"))
      case Command.Divide(dividend, divisor) => Right(Data(dividend/divisor))
      case Command.Average(xs)               => Right(Data(xs.sum/xs.length))
      case Command.Sum(xs)                   => Right(Data(xs.sum))
      case Command.Min(xs)                   => Right(Data(xs.min))
      case Command.Max(xs)                   => Right(Data(xs.max))
      case Command.ChainFraction(xs)         => if (xs.filter(_==0) == Nil)
                                                   Right(Data(xs.foldRight(Double.PositiveInfinity)(_+1.0/_)))
                                                   else Left(ErrorMessage("Division by zero"))
      case _                                 => Left(ErrorMessage(s"No calculation pattern for $x command"))
    }

  def renderResult(x: Result): String = x match {
    case Data(res) => res.toString
    case _         => "Error: Result cannot be rendered!"
  }

  def process(input: String): String = {
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    def eval(input: String) = for {
      x <- parseCommand(input)
      y <- calculate(x)
    } yield y

    eval(input) match {
      case Right(Data(a)) => renderResult(Data(a))
      case Left(ErrorMessage(a))  => "Error: " + a
      case _  => "Error: Unknown exception!"
    }

    // implement using a for-comprehension
  }


  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}








