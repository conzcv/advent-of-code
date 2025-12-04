package `2025`

import cats.effect.IO
import cats.effect.testing.utest.EffectTestSuite
import cats.syntax.all.*
import fs2.Stream
import shared.Task
import utest.*

import scala.concurrent.duration.*

import day1.Day1

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
      for answer <- (new Day1.Part1).of(Task(input))
      yield assert(answer == "3")
    }
    test("part2") {
      for answer <- (new Day1.Part2).of(Task(input))
      yield assert(answer == "6")
    }
  }
