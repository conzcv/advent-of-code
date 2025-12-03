package `2025`

import `2025`.day3.Day3
import `2025`.day3.models.Bank
import cats.effect.IO
import cats.effect.testing.utest.EffectTestSuite
import cats.syntax.all.*
import fs2.Stream
import shared.Task
import utest.*

import scala.concurrent.duration.*

object Day3Tests extends EffectTestSuite[IO]:
  override val timeout = 1.second

  val inputData: List[String] =
    List(
      "987654321111111",
      "811111111111119",
      "234234234234278",
      "818181911112111"
    )

  val input: Stream[IO, Byte] =
    Stream
      .iterable(inputData)
      .covary[IO]
      .intersperse("\n")
      .through(fs2.text.utf8.encode)

  val tests = Tests {
    test("part1") {
      for answer <- Day3.CommonSolution[IO](2).of(Task(input))
      yield assert(answer == "357")
    }

    test("part2") {
      for answer <- Day3.CommonSolution[IO](12).of(Task(input))
      yield assert(answer == "3121910778619")
    }
  }
