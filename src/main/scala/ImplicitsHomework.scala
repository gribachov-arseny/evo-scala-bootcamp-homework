package com.evolutiongaming.bootcamp.typeclass


import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {

    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        val sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }
    }

    import instances._
    import syntax._

    val b: Byte = 100
    b.sizeScore

    val xs: List[Byte] = List(1,2,3)
    xs.sizeScore

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        val delta = key.sizeScore + value.sizeScore
        val butLast = map.take(map.size-1)

        if ( delta <= maxSizeScore - map.sizeScore )
           map addOne (key,value)
        else
          if (map.nonEmpty)
            butLast.put(key,value)
          else ???

      }

      def get(key: K): Option[V] = map get key

    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }
    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator //f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!
      implicit val mapIterate: Iterate2[mutable.Map] = new Iterate2[mutable.Map] {
        override def iterator1[T, S](f: mutable.Map[T, S]): Iterator[T] = f.keysIterator
        override def iterator2[T, S](f: mutable.Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val packedMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner map {case (k,_) => k} iterator
        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner map {case (_,v) => v} iterator
      }

      /* ??? */ // Generalized: Iterator2 -> SizeScore
      implicit def iterate2ToGetSizeScore[M[_,_]: Iterate2,K,V]: GetSizeScore[M[K,V]] = { //GetSizeScore[Iterate2[M[K,V]]] = {
        //self => { ~ iter2.sizeScore + iter1.sizeScore }
        ???
        // how to extract prototype M[K,V] instance for Iterate2[M[K,V]]?
        // or there is another way ... 
        // question about search strategy for implicits. 
        // I do not want implement straightforward folds for proposed M[_,_] types. There must be general way.
      }

      /*
      replace this big guy with proper implicit instances for types:
      + Byte, Char, Int, Long
      + String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)
      */

      val p: SizeScore = 12   // <- pointer size

      implicit def stubGetSizeScore[T]: GetSizeScore[T] = (_: T) => 42

      implicit val byteToGetSizeScore: GetSizeScore[Byte] = (_: Byte) => 1
      implicit val intToGetSizeScore:  GetSizeScore[Int]  = (_: Int)  => 4
      implicit val longToGetSizeScore: GetSizeScore[Long] = (_: Long) => 8
      implicit val charToGetSizeScore: GetSizeScore[Char] = (_: Char) => 2
      implicit val strToGetSizeScore:  GetSizeScore[String] = (s: String) => p + 2*s.length


      //both: mutable & immutable
      // some other implicit: Collection[X] -> Iterator[X]
      // this       implicit: Iterator[X]   -> SizeScore
      implicit def iteratorToSizeScore[T]: GetSizeScore[Iterator[T]] = {
        (self: Iterator[T]) => self.foldRight(p) { (x,acc) => acc+x.sizeScore }
      }

      /*
      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */

      //obsolete:
      // implicits: Collection[X] -> Iterator[X]
      //implicit def listToIter[T]:  List[T]      =>    Iterator[T] = _.iterator
      //implicit def seqToIter[T]:   Seq[T]       =>    Iterator[T] = _.iterator
      //implicit def setToIter[T]:   Set[T]       =>    Iterator[T] = _.iterator
      //implicit def vectToIter[T]:  Vector[T]    =>    Iterator[T] = _.iterator
      //implicit def someToIter[C <: IterableOnce[_] ,  T]: Array[T] => Iterator[T] = _.iterator


    }
  }



  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._

    final case class Twit(
      id: Long,
      userId: Int,
      hashTags: Vector[String],
      attributes: PackedMultiMap[String, String],
      fbiNotes: List[FbiNote],
    )

    final case class FbiNote(
      month: String,
      favouriteChar: Char,
      watchedPewDiePieTimes: Long,
    )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = {
        import instances._

          val cache = new MutableBoundedCache[Long,Twit](maxSizeScore)

          new TwitCache {
            override def put(twit: Twit): Unit = cache.put(twit.id,twit)
            override def get(id: Long): Option[Twit] = cache.get(id)
          }

    }

  }

}
