package `2025`.day1

import cats.effect.Concurrent
import shared.Solution
import shared.Task
import fs2.Pipe
import fs2.text._
import cats.syntax.all._
import `2025`.day1.models.Rotation

object Day1:
  def parse[F[_]]: Pipe[F, Byte, Rotation] =
    _.through(utf8.decode andThen lines).map(Rotation.fromString)

  final class Part1[F[_]: Concurrent] extends Solution[F]:
    type Accumulator = (zeros: Int, rotation: Rotation)
    val initial: Accumulator = (0, Rotation.fromString("R50"))
    def of(task: Task[F]): F[String] =
      task.input
        .through(parse)
        .compile
        .fold(initial) { case ((zeros, rotated), current) =>
          val sum = rotated |+| current
          if (sum.angle == 0) (zeros + 1, sum)
          else (zeros, sum)
        }
        .map(_.zeros.toString)

  final class Part2[F[_]: Concurrent] extends Solution[F]:
    val initial: Rotation = Rotation.fromString("R50")
    def of(task: Task[F]): F[String] =
      task.input
        .through(parse)
        .cons1(initial)
        .compile
        .foldMonoid
        .map(_.turns.toString())
