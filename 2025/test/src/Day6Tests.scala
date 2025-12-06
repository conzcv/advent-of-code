package `2025`

import cats.effect.IO
import cats.effect.testing.utest.EffectTestSuite
import cats.syntax.all.*
import fs2.Stream
import shared.Task
import utest.*

import scala.concurrent.duration.*

import `2025`.day6.{Part1, Part2}

object Day6Tests extends EffectTestSuite[IO]:
  override val timeout = 1.second

  val inputData: List[String] =
    List(
      "123 328  51 64 ",
      " 45 64  387 23 ",
      "  6 98  215 314",
      "*   +   *   +  "
    )

  val input: Stream[IO, Byte] =
    Stream
      .iterable(inputData)
      .covary[IO]
      .intersperse("\n")
      .through(fs2.text.utf8.encode)

  val tests = Tests {
    test("part1") {
      for answer <- (new Part1).of(Task(input))
      yield assert(answer == "4277556")
    }

    test("part2") {
      for answer <- (new Part2).of(Task(input))
      yield assert(answer == "3263827")
    }
  }
