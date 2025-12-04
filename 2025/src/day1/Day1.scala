package `2025`.day1

import `2025`.day1.models.Rotation
import cats.syntax.all.*
import cats.effect.IO
import fs2.Pipe
import fs2.text.*
import shared.Solution
import shared.Task

object Day1:
  def parse: Pipe[IO, Byte, Rotation] =
    _.through(utf8.decode andThen lines).map(Rotation.fromString)

  final class Part1 extends Solution[IO]:
    type Accumulator = (zeros: Int, rotation: Rotation)
    val initial: Accumulator = (0, Rotation.fromString("R50"))
    def of(task: Task[IO]): IO[String] =
      task.input
        .through(parse)
        .compile
        .fold(initial) { case ((zeros, rotated), current) =>
          val sum = rotated |+| current
          if (sum.angle == 0) (zeros + 1, sum)
          else (zeros, sum)
        }
        .map(_.zeros.toString)

  final class Part2 extends Solution[IO]:
    val initial: Rotation = Rotation.fromString("R50")
    def of(task: Task[IO]): IO[String] =
      task.input
        .through(parse)
        .cons1(initial)
        .compile
        .foldMonoid
        .map(_.turns.toString())
