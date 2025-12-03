package `2025`.day3

import `2025`.day3.models.Bank
import `2025`.day3.models.Puzzle
import cats.effect.Concurrent
import cats.syntax.functor.*
import fs2.Pipe
import fs2.text.*
import shared.Solution
import shared.Task

object Day3:
  opaque type Index = Int

  def parse[F[_]]: Pipe[F, Byte, Bank] =
    _.through(utf8.decode andThen lines).collect(Bank.fromString)

  def initialState(lastIndex: Int): Puzzle[Index] =
    if (lastIndex >= 1) Puzzle.After(lastIndex, initialState(lastIndex - 1))
    else Puzzle.Init(0)

  def moveIndexToBestPosition(in: Vector[Short]): Index => Index =
    currentIndex =>
      if (currentIndex < in.length - 1)
        val nextIndex = moveIndexToBestPosition(in)(currentIndex + 1)
        val current = in(currentIndex)
        val next = in(nextIndex)
        if (next >= current) nextIndex else currentIndex
      else currentIndex

  def solve(data: Vector[Short]): Puzzle[Index] => Puzzle[Index] =
    case Puzzle.After(current, prev) =>
      val bestIndex: Int = moveIndexToBestPosition(data)(current)
      val underlying: Puzzle[Int] = solve(data.take(bestIndex))(prev)
      Puzzle.After(bestIndex, underlying)
    case Puzzle.Init(current) =>
      val bestIndex: Int = moveIndexToBestPosition(data)(current)
      Puzzle.Init(bestIndex)

  def extractAnswer(data: Vector[Short]): Puzzle[Index] => String =
    case Puzzle.Init(index) =>
      data(index).toString()
    case Puzzle.After(index, prev) =>
      data(index).toString() ++ extractAnswer(data)(prev)

  def getJoltage(outputs: Int): Bank => Long =
    case Bank(batteries) =>
      val reverse = batteries.reverse
      val getAnswer: Puzzle[Int] => String =
        solve(reverse) andThen extractAnswer(reverse)

      val unsolved: Puzzle[Int] = initialState(outputs - 1)
      getAnswer(unsolved).toLong

  final class CommonSolution[F[_]: Concurrent](outputs: Int)
      extends Solution[F]:
    def of(task: Task[F]): F[String] =
      task.input
        .through(parse)
        .map(getJoltage(outputs))
        .compile
        .foldMonoid
        .map(_.toString())
