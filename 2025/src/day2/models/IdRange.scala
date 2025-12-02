package `2025`.day2.models

final case class IdRange(from: String, to: String):
  /** Splits range to several ranges of same degree [1, 234] ->
    * [[1, 9], [10, 99], [100, 234]]
    */
  def split: LazyList[IdRange] =
    val maxDegree = to.length()
    LazyList.unfold(from) { start =>
      val degree = start.length()
      if (start.length() < maxDegree)
        Some(IdRange(start, "9".repeat(degree)), "1" + "0".repeat(degree))
      else if (start.length() == maxDegree) Some(IdRange(start, to), to ++ "0")
      else None
    }

  def invalids: LazyList[String] =
    split.flatMap {
      case IdRange(from, to) if from.size % 2 != 0 => LazyList.empty
      case IdRange(from, to)                       =>
        val (min1, min2) = from.splitAt(from.size / 2)
        val (max1, max2) = to.splitAt(from.size / 2)
        val min =
          if (min1.toLong < min2.toLong) min1.toLong + 1 else min1.toLong
        val max =
          if (max1.toLong > max2.toLong) max1.toLong - 1 else max1.toLong
        LazyList.from(min to max).map(long => long.toString().repeat(2))
    }

  private def primeFactors(n: Int): List[Int] = // could be better, i know
    LazyList
      .from(2)
      .takeWhile(_ <= n)
      .filter(n % _ == 0)
      .filter(d => (2 until d).forall(d % _ != 0))
      .toList

  def invalids2: LazyList[Long] =
    split.flatMap {
      case IdRange(from, to) if from.size > 1 =>
        val factors = primeFactors(from.length())
        val fromPrimes: LazyList[Long] =
          if (factors.size == 1 && factors.head == from.size)
            LazyList.empty
          else
            for
              divisor <- LazyList.from(factors)
              divident = from.length() / divisor
              min = from.take(divident)
              max = to.take(divident)
              invalid <- LazyList
                .from(min.toLong to max.toLong)
                .map(_.toString())
                .filter(_.toSet.size > 1)
                .map(_.repeat(divisor).toLong)
            yield invalid

        val fromUnit: LazyList[Long] =
          val min = from.take(1)
          val max = to.take(1)
          LazyList
            .from(min.toLong to max.toLong)
            .map(_.toString().repeat(from.size).toLong)

        val fromLong = from.toLong
        val toLong = to.toLong

        (fromPrimes concat fromUnit)
          .filter(l => l >= fromLong && l <= toLong)
      case _ => LazyList.empty
    }
