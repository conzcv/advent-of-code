package `2025`.day2

import shared.Solution
import shared.Task

import fs2.Stream
import `2025`.day2.models.IdRange
import cats.effect.Concurrent
import cats.syntax.functor._

object Day2:
  final class Part1[F[_]: Concurrent] extends Solution[F]:
    def of(task: Task[F]): F[String] =
      task.input
        .through(Parser.tokens)
        .collect(Parser.range)
        .map(_.invalids)
        .flatMap(Stream.iterable)
        .map(_.toLong)
        .compile
        .foldMonoid
        .map(_.toString())
