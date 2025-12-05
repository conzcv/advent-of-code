package `2025`.day5.models

import fs2.Chunk
import cats.kernel.Monoid
import cats.syntax.foldable._
import scala.annotation.tailrec

final class DisjointUnion private (val toList: List[Interval]):
  private def addStep(list: List[Interval], toAdd: Interval): List[Interval] =
    list match
      case head :: tail if head.intersects(toAdd) =>
        val start = head.start.min(toAdd.start)
        val end = head.end.max(toAdd.end)
        val union = Interval.unsafe(start, end)
        addStep(tail, union)
      case head :: tail if head.start < toAdd.start =>
        head :: addStep(tail, toAdd)
      case head :: tail =>
        toAdd :: head :: tail
      case Nil =>
        List(toAdd)

  def add(interval: Interval): DisjointUnion =
    new DisjointUnion(addStep(toList, interval))

  def contains(long: Long): Boolean =
    toList.exists(_.contains(long))

  def size: Long = toList.foldMap(_.size)

object DisjointUnion:
  def singleton(interval: Interval): DisjointUnion =
    new DisjointUnion(List(interval))
