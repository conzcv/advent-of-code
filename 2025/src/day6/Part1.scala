package `2025`.day6

import `2025`.day6.models.part1.InputData
import `2025`.day6.models.Line
import cats.Monoid
import cats.data.NonEmptyVector
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.foldable.*
import fs2.Pipe
import fs2.text.*
import shared.Solution
import shared.Task

final class Part1 extends Solution[IO]:
  val parseLines: Pipe[IO, Byte, String] =
    utf8.decode andThen lines
  def of(task: Task[IO]): IO[String] =
    task.input
      .through(parseLines)
      .filter(_.nonEmpty)
      .compile
      .toVector
      .flatMap(lines =>
        InputData.parse(lines) match
          case Right(input) =>
            given Monoid[Line] = input.monoidInstance
            input.lines.combineAll.sum.toString.pure[IO]
          case Left(error) =>
            IO.raiseError(new Exception(error))
      )
