package `2025`.day5

import shared.Solution
import cats.effect.IO
import shared.Task

import cats.syntax.foldable._

import models.InputData

final class Part2 extends Solution[IO]:
  def of(task: Task[IO]): IO[String] =
    Common.parse(task.input).map {
      case InputData(identifiers, Some(intervals)) =>
        intervals.size.toString
      case InputData(intervals, None) => "0"
    }
