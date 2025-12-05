package `2025`.day5

import `2025`.day5.models.Interval
import fs2.Stream
import cats.effect.IO
import `2025`.day5.models.InputData
import fs2.text._
import `2025`.day5.models.DisjointUnion

object Common:
  val IntervalPattern = "^(\\d+)-(\\d+)$".r

  extension [A](vector: List[A])
    def collectWhile[B](pf: PartialFunction[A, B]): List[B] =
      vector.map(pf.lift).takeWhile(_.nonEmpty).flatten

  val split: PartialFunction[String, (Long, Long)] =
    case IntervalPattern(fst, snd) => (fst.toLong, snd.toLong)

  val build: PartialFunction[(Long, Long), Interval] =
    Function.unlift(Interval.build)

  def parse(bytes: Stream[IO, Byte]): IO[InputData] =
    bytes.through(utf8.decode andThen lines).compile.toList.map { lines =>
      val intervals: List[Interval] = lines.collectWhile(split andThen build)

      val union: Option[DisjointUnion] =
        intervals match
          case Nil          => None
          case head :: tail =>
            val start: DisjointUnion = DisjointUnion.singleton(head)
            Some(tail.foldLeft(start)(_.add(_)))

      val identifiers: List[Long] =
        lines.drop(intervals.length + 1).dropRight(1).map(_.toLong)

      InputData(identifiers, union)
    }
