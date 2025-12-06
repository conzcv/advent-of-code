package `2025`.day6

import `2025`.day6.models.Line
import cats.Monoid
import cats.data.NonEmptyVector
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.foldable.*
import fs2.Pipe
import fs2.text.*
import shared.Solution
import shared.Task
import cats.instances.list

enum Op:
  case Plus(size: Int)
  case Times(size: Int)
  def increment: Op = this match
    case Plus(size)  => Plus(size + 1)
    case Times(size) => Times(size + 1)

  def decrement: Op = this match
    case Plus(size)  => Plus(size - 1)
    case Times(size) => Times(size - 1)

final class Part2 extends Solution[IO]:
  val parseLines: Pipe[IO, Byte, String] =
    utf8.decode andThen lines

  def structure(operations: List[Char], prev: Option[Op] = None): List[Op] =
    (operations, prev) match
      case ('+' :: tail, Some(op)) =>
        op.decrement :: structure(tail, Some(Op.Plus(1)))
      case ('+' :: tail, None)     => structure(tail, Some(Op.Plus(1)))
      case ('*' :: tail, Some(op)) =>
        op.decrement :: structure(tail, Some(Op.Times(1)))
      case ('*' :: tail, None)     => structure(tail, Some(Op.Times(1)))
      case (' ' :: tail, Some(op)) => structure(tail, Some(op.increment))
      case (' ' :: tail, None)     => structure(tail, None)
      case (Nil, Some(op))         => op :: Nil
      case _                       => Nil

  private def parseSegment(
      size: Int,
      data: Vector[String]
  ): (IndexedSeq[Long], Vector[String]) =
    val (numbers, next) = data.map(_.splitAt(size)).unzip
    val segment = (0 until size).map(index =>
      numbers
        .map(_.apply(index))
        .foldLeft("") {
          case (accumulator, ' ') =>
            accumulator
          case (accumulator, char) =>
            accumulator.appended(char)
        }
        .toLong
    )
    (segment, next.map(_.drop(1)))

  def numbers(ops: List[Op], data: Vector[String]): List[Long] =
    ops match
      case Op.Plus(size) :: tail =>
        val (segment, next) = parseSegment(size, data)
        val result: Long = segment.foldLeft(0L)(_ + _)
        result :: numbers(tail, next)
      case Op.Times(size) :: tail =>
        val (segment, next) = parseSegment(size, data)
        val result: Long = segment.foldLeft(1L)(_ * _)
        result :: numbers(tail, next)
      case Nil => Nil

  def solve(vector: Vector[String]): List[Long] =
    val ops: List[Op] = structure(vector.last.toList)
    val data: Vector[String] = vector.dropRight(1)

    numbers(ops, data)

  def of(task: Task[IO]): IO[String] =
    task.input
      .through(parseLines)
      .filter(_.nonEmpty)
      .compile
      .toVector
      .map { vector =>
        val solution = solve(vector)
        solution.combineAll.toString()
      }
