package `2025`.day2

import `2025`.day2.models.IdRange
import cats.effect.IO
import cats.syntax.functor.*
import fs2.Stream
import shared.Solution
import shared.Task

object Day2:
  final class Part1 extends Solution[IO]:
    def of(task: Task[IO]): IO[String] =
      task.input
        .through(Parser.tokens)
        .collect(Parser.range)
        .map(_.invalids)
        .flatMap(Stream.iterable)
        .map(_.toLong)
        .compile
        .foldMonoid
        .map(_.toString())

  final class Part2 extends Solution[IO]:
    def of(task: Task[IO]): IO[String] =
      task.input
        .through(Parser.tokens)
        .collect(Parser.range)
        .map(_.invalids2)
        .flatMap(Stream.iterable)
        .compile
        .foldMonoid
        .map(_.toString())
