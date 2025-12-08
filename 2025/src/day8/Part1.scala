package `2025`.day8

import `2025`.day8.models.Box
import `2025`.day8.models.Circuit
import `2025`.day8.models.Edge
import cats.effect.IO
import cats.syntax.semigroup.*
import fs2.Pipe
import fs2.text.*
import shared.Solution
import shared.Task

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

final class Part1(depth: Int) extends Solution[IO]:
  val collectBoxes: Pipe[IO, String, Box] =
    _.collect(Box.fromString)

  val parseLines: Pipe[IO, Byte, String] =
    utf8.decode andThen lines

  def connect(
      edges: List[Edge],
      circuits: SortedMap[Box, Circuit] = SortedMap.empty
  ): SortedMap[Box, Circuit] =
    edges match
      case edge :: next =>
        (circuits.get(edge.first), circuits.get(edge.second)) match
          case (Some(fst), Some(snd)) if fst != snd =>
            val circuit = fst |+| snd
            val updated = circuits ++ circuit.toChain
              .map(box => box -> circuit)
              .toNem
              .toSortedMap
            connect(next, updated)
          case (Some(fst), Some(snd)) =>
            connect(next, circuits)
          case (Some(fst), None) =>
            val circuit = fst |+| Circuit.one(edge.second)
            val updated = circuits ++ circuit.toChain
              .map(box => box -> circuit)
              .toNem
              .toSortedMap
            connect(next, updated)
          case (None, Some(snd)) =>
            val circuit = Circuit.one(edge.first) |+| snd
            val updated = circuits ++ circuit.toChain
              .map(box => box -> circuit)
              .toNem
              .toSortedMap
            connect(next, updated)
          case (None, None) =>
            val circuit = Circuit.one(edge.first) |+| Circuit.one(edge.second)
            val updated = circuits ++ circuit.toChain
              .map(box => box -> circuit)
              .toNem
              .toSortedMap
            connect(next, updated)
      case Nil => circuits

  def of(task: Task[IO]): IO[String] =
    task.input
      .through(parseLines andThen collectBoxes)
      .compile
      .toVector
      .map { boxes =>
        val edges = for
          a <- LazyList.from(boxes)
          b <- LazyList.from(boxes)
          if a != b
        yield Edge.create(a, b)

        val sorted: SortedSet[Edge] = SortedSet.from(edges)

        connect(sorted.take(depth).toList).values
          .map(_.size)
          .toList
          .distinct
          .sorted(using Ordering[Long].reverse)
          .take(3)
          .foldLeft(1L)(_ * _)
          .toString()
      }
