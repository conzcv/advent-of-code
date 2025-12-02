package `2025`

import scala.concurrent.duration._
import utest._
import cats.syntax.all._
import cats.effect.IO
import cats.effect.testing.utest.EffectTestSuite
import fs2.Stream
import `2025`.day2.Parser
import `2025`.day2.models.IdRange
import `2025`.day2.Day2
import shared.Task

object Day2Tests extends EffectTestSuite[IO]:
  override val timeout = 1.second

  val inputData: String =
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224," +
      "1698522-1698528,446443-446449,38593856-38593862,565653-565659," +
      "824824821-824824827,2121212118-2121212124"

  val input: Stream[IO, Byte] =
    Stream
      .emit(inputData)
      .through(fs2.text.utf8.encode)
      .rechunkRandomly()
      .covary[IO]

  val tests = Tests {
    test("parse") {
      input
        .through(Parser.tokens)
        .intersperse(",")
        .compile
        .foldMonoid
        .map(result => assert(result == inputData))
    }

    test("split") {
      val result = IdRange("3", "12357").split
      val expected = List(
        IdRange("3", "9"),
        IdRange("10", "99"),
        IdRange("100", "999"),
        IdRange("1000", "9999"),
        IdRange("10000", "12357")
      )
      IO(assert(result == expected))
    }

    test("part1") {
      Day2
        .Part1[IO]
        .of(Task(input))
        .map(answer => assert(answer == "1227775554"))
    }
  }
