package `2025`.day5.models

import scala.collection.immutable.NumericRange

final class Interval private (val start: Long, val end: Long):
  def intersects(other: Interval): Boolean =
    contains(other.start) || other.contains(start)

  def contains(long: Long): Boolean =
    start <= long && long <= end

  def size: Long = 1L + end - start

  override def toString: String = s"[$start, $end]"

object Interval:
  def unsafe(start: Long, end: Long): Interval =
    new Interval(start, end)

  def build(start: Long, end: Long): Option[Interval] =
    Option.when(start <= end)(new Interval(start, end))
