package `2025`.day6.models.part1

import cats.data.NonEmptyVector
import cats.syntax.traverse._
import cats.kernel.Monoid
import `2025`.day6.models.Line
import `2025`.day6.models.Operation

final class InputData private (
    val lines: Vector[Line],
    operations: NonEmptyVector[Operation]
):

  val monoidInstance: Monoid[Line] =
    new:
      val zipper: Line => Line => Line =
        Line.zipper(operations)

      def combine(x: Line, y: Line): Line = zipper(x)(y)

      val empty: Line = Line.empty(operations)

object InputData:
  def split(string: String): Vector[String] =
    string.split("\\s+").filter(_.nonEmpty).toVector

  def parse(vector: Vector[String]): Either[String, InputData] =
    vector.reverse.splitAt(1) match
      case (Vector(ops), nums) =>
        val operations: Vector[Operation] =
          split(ops).collect(Operation.fromString)

        for
          operations <- NonEmptyVector
            .fromVector(operations)
            .toRight("operations list shouldn't be empty")
          lines <- nums.traverse(Line.fromString(_, operations.length))
        yield new InputData(lines, operations)
      case _ =>
        Left("at least one line is expected")
