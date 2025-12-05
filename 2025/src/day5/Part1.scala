package `2025`.day5

import cats.effect.IO
import cats.syntax.foldable.*
import shared.Solution
import shared.Task

import models.InputData

final class Part1 extends Solution[IO]:
  def of(task: Task[IO]): IO[String] =
    Common.parse(task.input).map {
      case InputData(identifiers, Some(intervals)) =>
        identifiers.count(intervals.contains).toString()
      case InputData(intervals, None) => "0"
    }
