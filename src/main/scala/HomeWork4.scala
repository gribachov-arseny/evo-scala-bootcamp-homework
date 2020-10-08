package main.scala

object HomeWork4 {



  /** Recursion Exercises */



  /** Misc functions */

  // it groups a list "xs" by a predicate "p"
  def groupBy[A]: (A => A => Boolean) => List[A] => List[List[A]] = { p => {
    case Nil => Nil
    case a :: as =>
      (a :: as.takeWhile(p(a)(_))) :: groupBy(p)(as.dropWhile(p(a)(_)))
  }  }

  // function head wrapped into List container
  def headInList[A]: List[A] => List[A] = {
    case x :: _  => List(x)
    case Nil     => Nil
  }

  // function tail wrapped into List container
  def tailInList[A]: List[A] => List[List[A]] = {
    case _ :: xs if xs.nonEmpty  => List(xs)
    case _                       => Nil
  }

  def iterate[A]: (A => A) => Int => A => A = {
    f => k => x => k match {
      case k if k > 0 => iterate(f)(k-1)(f(x))
      case _          => x
    }
  }

  def zipWith[A,B,C](f: A => B => C)(as: List[A])(bs: List[B]): List[C] = (as,bs) match {
    case (Nil,_)       => Nil
    case (_,Nil)       => Nil
    case (x::xs,y::ys) => f(x)(y) :: zipWith(f)(xs)(ys)
  }

  /** Zip-Subsequences */

   // all binary masks with k '1's from 0 to n-1
  def binaries(k: Int)(n: Int): List[String] = {

    def powOf2(x: Int): Int = Math.pow(2,x).toInt

    def toBinary(n: Int)(i: Int): String = {
      val b = i.toBinaryString
      val z = n - b.length

      "0"*z ++ b
    }

    for {
      i <- (0 until powOf2(n)).toList
      b = toBinary(n)(i)
      if b.filter(_=='1').length == k
    } yield b

  }


  // Zip masks with binaries to get subSequences
  def subSeqs[A](k: Int)(xs: List[A]): List[List[A]] = {
    val binaryList = binaries(k)(xs.length) map (_.toList)
    val chose: Char => A => List[A] = m => x => if (m=='1') List(x) else Nil

    val temp = for (mask <- binaryList)
      yield zipWith(chose)(mask)(xs)

    temp map (_.flatten)

  }

  /**
   * Permutations of a List
   *
   */

  // e.g. [a,b] -> [[x,a,b],[a,x,b],[a,b,x]]
  def insertions[A]: A => List[A] => List[List[A]] = x => xs =>
    for {i <- (0 to xs.length).toList
         (face, back) = ( xs.take(i), xs.drop(i) ) }
      yield face ++ (x :: back)

  def perms[A]: List[A] => List[List[A]] = {
    case Nil      => List(Nil)
    case x :: xs  => perms(xs) flatMap (insertions(x)(_))
  }

  /**
   *  Subsequences of the List:
   *  - order matters.
   *  - subSequenceOfLength(k)(n).length = n!/(n-k)!
   *  - if k > n then Nil
   */

  //one iteration
  def partitions[A,B]: (List[A],List[A]) => List[(List[A],List[A])] = {
    (acc, rem) => {
      for {i <- rem.indices.toList
           (a, b) = rem.splitAt(i)}
        yield (rem(i) :: acc, (a ++ b.tail))
    }
  }

  def subSequenceOfLength[A](k: Int)(ls: List[A]): List[List[A]] = {

    def go(k: Int)(arg: List[A]): List[(List[A], List[A])]
    = iterate[List[(List[A], List[A])]](xs => xs flatMap (partitions.tupled(_)) )(k)(List((Nil,arg)))

    go(k)(ls) map (_._1)
  }



  /**
   * Get change : Returns a list of all possible ways to exchange money
   *              with coins of fixed denominations
   */

  val coins: List[Int] = List(2,3,7)

  def change: Int => List[List[Int]] = {
    case n if n < 0  => List()
    case n if n == 0 => List(Nil)
    case n => for { x <- coins; xs <- change(n-x) } yield (x :: xs)
  }


  /**
   * Transpose : It intеrchanges columns and rows for List of Lists
   *             Lists can have arbitrary length.
   */

  def transpose[A]: List[List[A]] => List[List[A]] = {

    def safeHeads(xs: List[List[A]]): List[A] = xs flatMap headInList[A]
    def safeTails(xs: List[List[A]]): List[List[A]] = xs flatMap tailInList[A]

    { case Nil    => Nil
      case xss    => safeHeads(xss) :: transpose( safeTails(xss) ) }

  }

  /** The Main Exercise */

  def sortConsideringEqualValues[T](m: Map[T, Int]): List[(Set[T], Int)] = {
    val sorted = m.toList.sortWith( _._2 < _._2 )
    val grouped = groupBy[(T, Int)](a => b => (a._2 == b._2))(sorted)

    grouped.map( t => t.map(_._1).toSet -> t.head._2 )

  }

  //TODO: Write Scala Tests.

}
