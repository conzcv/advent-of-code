package `2025`.day6.models

import cats.data.NonEmptyVector
import cats.syntax.foldable.*
import cats.syntax.traverse.*

opaque type Line = NonEmptyVector[Long]

object Line:
  private val parseLong: String => Either[String, Long] =
    s => s.toLongOption.toRight(s"$s should be a number")

  extension (l: Line)
    def sum: Long = l.combineAll
    def underlying: NonEmptyVector[Long] = l

  def fromString(string: String, size: Int): Either[String, Line] =
    for
      longs <- string
        .split("\\s+")
        .filter(_.nonEmpty)
        .toVector
        .traverse(parseLong)
      result <- NonEmptyVector
        .fromVector(longs)
        .toRight("number line shouldn't be empty")
      _ <- Either.cond(
        result.length == size,
        (),
        s"expected line size is $size, not ${result.length}"
      )
    yield result

  def zipper(operations: NonEmptyVector[Operation]): Line => Line => Line =
    l1 =>
      l2 =>
        val functions: NonEmptyVector[Long => Long] =
          operations.zipWith(l1) {
            case (Operation.Plus, number)  => _ + number
            case (Operation.Times, number) => _ * number
          }
        functions.zipWith(l2)((f, n) => f(n))

  def empty(operations: NonEmptyVector[Operation]): Line =
    operations.map {
      case Operation.Plus  => 0
      case Operation.Times => 1
    }
