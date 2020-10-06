package com.evolutiongaming.bootcamp.basics

import java.io.FileNotFoundException

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

import java.time.Month

object ControlStructures {
  // You can follow your progress using the tests in `ControlStructuresSpec`.

  // The if-then-else construct is as follows:
  //
  // val result =
  //   if (boolean1) {
  //     result1
  //   } else if (boolean2) {
  //     result2
  //   } else {
  //     otherResult
  //   }
  //
  // Note that it returns a result value.

  // Note that curly braces can be omitted
  //  val result =
  //    if (boolean1) result1
  //    else if (boolean2) result2
  //    else otherResult

  //TODO
  // Exercise. Implement a "Fizz-Buzz" https://en.wikipedia.org/wiki/Fizz_buzz function using the if-then-else,
  // returning "fizzbuzz" for numbers which divide with 15, "fizz" for those which divide by 3 and "buzz" for
  // those which divide with 5, and returning the input number as a string for other numbers:
  def fizzBuzz1(n: Int): String = {
    if (n % 15 == 0) "fizzbuzz"
    else if (n % 3 == 0) "fizz"
    else if (n % 5 == 0) "buzz"
    else n.toString
  }

  // Pattern Matching
  //
  // Using the match-case construct we can write constructs equivalent to if-then-else statements in, often,
  // a more readable and concise form:
  //
  // val result = someValue match {
  //    case pattern1                       => result1
  //    case pattern2 if (guardCondition)   => result2
  //    case _                              => fallbackResult
  // }

  type ErrorMessage = String // ← alias
  def monthName(x: Int): Either[ErrorMessage, String] = {

    if (x < 1) {
      Left(s"Month $x is too small")
    } else if (x > 12) {
      Left(s"Month $x is too large")
    }
    else {
      val month = Month.of(x).toString
      Right(month.head + month.tail.toLowerCase)
    }

  }

  //TODO
  // Question. How would you improve `monthName`?
  ///an example is above; using java.time.Month
  //TODO
  // Question. What would you use in its place if you wanted to more properly handle multiple locales?
  /// I'm not sure I understand properly this question.

  //TODO
  // Exercise. Implement a "Fizz-Buzz" function using pattern matching:
  def fizzBuzz2(n: Int): String = {
    n match {
      case n if n % 15 == 0 => "fizzbuzz"
      case n if n % 3 == 0 => "fizz"
      case n if n % 5 == 0 => "buzz"
      case y => y.toString
    }
  }

  def fizzBuzz3(n: Int) = (n % 3, n % 5) match {
    case (0, 0) => "fizzbuzz"
    case (0, _) => "fizz"
    case (_, 0) => "buzz"
    case _ => n.toString
  }


  /** UP TO HERE */

  // Recursion
  //
  // A function which calls itself is called a recursive function. This is a commonly used way how to
  // express looping constructs in Functional Programming languages.

  def sum1(list: List[Int]): Int = {
    if (list.isEmpty) 0
    else list.head + sum1(list.tail)
  }

  //TODO
  // Question. What are the risks of recursion when applied without sufficient foresight?
  /// → infinite loops, functional divergences, memory devouring etc.

  // @tailrec annotation verifies that a method will be compiled with tail call optimisation.
  @tailrec
  def last[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ :: xs => last(xs)
  }

  // In reality, recursion isn't used that often as it can be replaced with `foldLeft`, `foldRight`,
  // `reduce` or other larger building blocks.

  def sum2(list: List[Int]): Int = {
    list.foldLeft(0)((acc, x) => acc + x)
  }

  def sum3(list: List[Int]): Int = {
    list.foldRight(0)((x, acc) => acc + x)
  }

  def sum4(list: List[Int]): Int = {
    if (list.isEmpty) 0
    else list.reduce((a, b) => a + b)
  }

  //TODO:
  // Question. How is List.sum implemented in the standard library?
  //->  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  //TODO:
  // Exercise: Implement a function `applyNTimes` which takes a function `f` and an integer `n` and
  // returns a function which applies the function `f` `n` times.
  // Thus `applyNTimesForInts(_ + 1, 4)(3)` should return `((((3 + 1) + 1) + 1) + 1)` or `7`.
  def applyNTimesForInts(f: Int => Int, n: Int): Int => Int = { x: Int =>
    n match {
      case 1 => f(x)
      case n if n > 0 => applyNTimesForInts(f, n - 1)(f(x))
      case _ => x // exception branch: id function
    }
  }


  //TODO:
  // Exercise: Convert the function `applyNTimesForInts` into a polymorphic function `applyNTimes`:
  def applyNTimes[A](f: A => A, n: Int): A => A = { x: A =>
    if (n > 0)
      n match {
        case 0 => x
        case _ => applyNTimes[A](f, n - 1)(f(x))
      } else x // exception branch: id function
  }

  // `map`, `flatMap` and `filter` are not control structures, but methods that various collections (and
  // not only collections) have. We will discuss them now as they are important to understand a key
  // control structure called "for comprehensions".

  // `map` is a higher order function which - in case of collections - transforms each element of the
  // collection into a different element (and returns the resulting collection)

  // For example, for `List` it is defined as
  object list_map_example { // name-spacing to not break other code in this lesson
    class List[A] {
      //def map[B](f: A => B): List[B] = f(this.head) :: map[B](f)(this.tail)
    }

  }

  //TODO
  // Question. What is the value of this code?
  val listMapExample = List(1, 2, 3).map(x => x * 2)

  //-> List(2,4,6)

  // As we will see in later lessons, `map` is a method that `Functor`-s have, and there are more `Functor`-s
  // than just collections (`IO`, `Future`, `Either`, `Option` are all `Functor`-s too).

  // For now, we will have a utilitarian focus and not go into `Functor`-s and other type classes.

  // `flatMap` is a higher order function which - for collections - transforms each element of the collection
  // into a collection, and then `flatten`-s these collections.

  // For example, for `List` it could be defined as:
  object list_flatmap_example { // name-spacing to not break other code in this lesson
    class List[A] {
      //def flatMap[B](f: A => List[B]): List[B] = this.map(f).foldRight(Nil)(_:::_)
    }

  }


  //TODO
  // Question. What is the value of this code?
  val listFlatMapExample = List(1, 2, 3).flatMap(x => List(x, x * 2))

  //List(1, 2, 2, 4, 3, 6): List[Int]

  //TODO
  // Question. Do you think only collections can have `flatMap`?
  /// -> Any Monad. flatMap = (=<<) reversed bind operator from Haskell

  // `filter` takes a predicate function returning a boolean and - for collections - returns a collection
  // with only these elements which satisfy this predicate.

  // For example, for `List` it is defined as:
  object list_filter_example {

    class List[A] {
      //def filter(p: A => Boolean): List[A] = this.flatMap[A]( (x:A) => if (p(x)) List(x) else Nil )
    }

  }

  // Question. What is the value of this code?
  val listFilterExample = List(1, 2, 3).filter(_ % 2 == 0)
  //-> List(2)


  // For Comprehensions


  // A `for-yield` syntax is syntactic sugar for composing multiple `map`, `flatMap` and `filter` operations
  // together in a more readable form.

  // val result = for {
  //   x <- a
  //   y <- b
  // } yield x + y
  //
  // gets translated to
  //
  // val result = a.flatMap(x => b.map(y => x + y))

  private val a = List(1, 2, 3)
  private val b = List(10, 100)

  val c = for {
    x <- a
    y <- b
  } yield x * y

  val d = a.flatMap(x => b.map(y => x * y))

  //TODO
  // Question: What is the value of `c` above?
  // -> List(10, 100, 20, 200, 30, 300): List[Int]
  // Question: What is the value of `d` above?
  // -> (c == d)

  // You can also add `if` guards to `for` comprehensions:
  val e = for {
    x <- a
    if x % 2 == 1
    y <- b
  } yield x + y

  //TODO
  // Question. What is the value of `e` above?
  //-> List(11, 101, 13, 103): List[Int]

  // In idiomatic functional Scala, much of the code ends up written in "for comprehensions".
  //TODO
  // Exercise. Implement `makeTransfer` using `for` comprehensions and the methods provided in `UserService`.
  //->

  type UserId = String
  type Amount = BigDecimal

  trait UserService {
    def validateUserName(name: String): Either[ErrorMessage, Unit]

    def findUserId(name: String): Either[ErrorMessage, UserId]

    def validateAmount(amount: Amount): Either[ErrorMessage, Unit]

    def findBalance(userId: UserId): Either[ErrorMessage, Amount]

    /** Upon success, returns the resulting balance */
    def updateAccount(userId: UserId, previousBalance: Amount, delta: Amount): Either[ErrorMessage, Amount]
  }

  //TODO
  // Upon success, should return the remaining amounts on both accounts (fromUser, toUser).
  def makeTransfer(service: UserService, fromUser: String, toUser: String, amount: Amount): Either[ErrorMessage, (Amount, Amount)] = {
    // Replace with a proper implementation:
    import service._
    for {
      _ <- validateUserName(fromUser) //?? is there (<<) analog? or always "_ <- ..." is written for forgetful binds?
      _ <- validateUserName(toUser)
      _ <- validateAmount(amount)
      senderID <- findUserId(fromUser)
      receiverID <- findUserId(toUser)
      senderBalance <- findBalance(senderID)
      receiverBalance <- findBalance(receiverID)
      newSender <- updateAccount(senderID, senderBalance, -amount)
      newReceiver <- updateAccount(receiverID, receiverBalance, amount)
    } yield (newSender, newReceiver)
  }

    //TODO
    // Question. What are the questions would you ask - especially about requirements - before implementing
    // this function? What potential issues can you run into?
    //TODO
    // Exercise. The tests for `makeTransfer` in `ControlStructuresSpec` are insufficient. Improve the tests.
    //TODO
    // Question. Does the implementation of `makeTransfer` depend on `Either` being the "container"?
    // -> No

    // Let us return to our "intuition about types" exercises from before.

    //TODO
    // Exercise:
    //
    // Given:
    //  A = Set(0, 1, 2)
    //  B = Set(true, false)
    //
    // List all the elements in `A * B`.
    //
    // Use a "for comprehension" in your solution.

    def allElemPairs[A, B]: Set[A] => Set[B] => Set[(A, B)] =
      xs => ys => for (x <- xs; y <- ys) yield (x, y)

    val AProductB = allElemPairs(Set(0, 1, 2))(Set(true, false))

    //TODO
    // Exercise:
    //
    // Given:
    // A = { 0, 1, 2 }
    // B = { true, false }
    //
    // List all the elements in `A + B`.
    //
    // Use "map" and `++` (`Set` union operation) in your solution.

    val ASumB: Set[Either[Int, Boolean]] = allElemEitherSet(Set(0, 1, 2))(Set(true, false))

    def allElemEitherSet[A, B]: Set[A] => Set[B] => Set[Either[A, B]] = {
      as => bs => as.map(Left(_)) ++ bs.map(Right(_)) toSet
    }


    // Scala inherits the standard try-catch-finally construct from Java:
    def printFile(fileName: String): Unit = {
      // This code is only here to illustrate try-catch-finally, it shouldn't be considered as good code
      val source = Source.fromFile(fileName)
      try { // executed until an exception happens
        source.getLines() foreach println
      } catch { // exception handlers
        case e: FileNotFoundException => println(s"Couldn't find the file: $e")
        case e: Exception => println(s"Exception occurred: $e")
      } finally { // executed even if an exception happens
        source.close
      }
    }

    //TODO
    // Question. What issues can you find with the above `printFile` method?
    //-> it's better to use pure functions. It will be easier to handle them in the future.

    // However, in idiomatic functional Scala, other error handling mechanisms are usually used.
    // Throwing exceptions is an anti-pattern - it introduces another exit path from a function and breaks
    // referential transparency.

    // One of them is `Try` which can be thought of as an `Either[Throwable, A]`:

    def parseInt2(x: String, default: Int = 0): Try[Int] = x.toIntOption.getOrElse(default)

    def parseInt1(x: String): Try[Int] = Try(x.toInt)

    parseInt1("asdf") match {
      case Success(value) => println(value)
      case Failure(error) => println(error)
    }

    //TODO
    // Question. What other ways of representing the "parse string to integer" results can you think of?
    //-> see HW, however, there is not the best realisation.
  }

