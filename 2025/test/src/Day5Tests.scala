package `2025`

import cats.effect.IO
import cats.effect.testing.utest.EffectTestSuite
import cats.syntax.all.*
import fs2.Stream
import shared.Task
import utest.*

import scala.concurrent.duration.*

import day5.{Part1, Part2}

object Day5Tests extends EffectTestSuite[IO]:
  override val timeout = 1.second

  val inputData: List[String] =
    List(
      "3-5",
      "10-14",
      "16-20",
      "12-18",
      "",
      "1",
      "5",
      "8",
      "11",
      "17",
      "32"
    )

  val input: Stream[IO, Byte] =
    Stream
      .iterable(inputData)
      .covary[IO]
      .intersperse("\n")
      .through(fs2.text.utf8.encode)

  val tests = Tests {
    test("part1") {
      (new Part1).of(Task(input)).map(_ == "3")
    }
    test("part2") {
      (new Part2).of(Task(input)).map(_ == "14")
    }
  }
