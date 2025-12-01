package `2025`

import scala.concurrent.duration._
import utest._
import cats.syntax.all._
import cats.effect.IO
import cats.effect.testing.utest.EffectTestSuite
import fs2.Stream
import day1.Day1
import shared.Task

object Day1Tests extends EffectTestSuite[IO]:
  override val timeout = 1.second

  val inputData: List[String] =
    List("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

  val input: Stream[IO, Byte] =
    Stream
      .iterable(inputData)
      .covary[IO]
      .intersperse("\n")
      .through(fs2.text.utf8.encode)

  val tests = Tests {
    test("part1") {
      for answer <- Day1.Part1[IO].of(Task(input))
      yield assert(answer == "3")
    }
    test("part2") {
      for answer <- Day1.Part2[IO].of(Task(input))
      yield assert(answer == "6")
    }
  }
