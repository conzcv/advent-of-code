package `2025`

import `2025`.day4
import cats.effect.IO
import cats.effect.testing.utest.EffectTestSuite
import cats.syntax.all.*
import fs2.Stream
import shared.Task
import utest.*

import scala.concurrent.duration.*

object Day4Tests extends EffectTestSuite[IO]:
  override val timeout = 1.second

  val inputData: List[String] =
    List(
      "..@@.@@@@.",
      "@@@.@.@.@@",
      "@@@@@.@.@@",
      "@.@@@@..@.",
      "@@.@@@@.@@",
      ".@@@@@@@.@",
      ".@.@.@.@@@",
      "@.@@@.@@@@",
      ".@@@@@@@@.",
      "@.@.@@@.@."
    )

  val input: Stream[IO, Byte] =
    Stream
      .iterable(inputData)
      .covary[IO]
      .intersperse("\n")
      .through(fs2.text.utf8.encode)

  val tests = Tests {
    test("part1") {
      for answer <- (new day4.Part1).of(Task(input))
      yield assert(answer == "13")
    }

    test("part2") {
      for answer <- (new day4.Part2).of(Task(input))
      yield assert(answer == "43")
    }
  }
