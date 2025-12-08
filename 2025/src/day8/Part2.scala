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

final class Part2 extends Solution[IO]:
  val collectBoxes: Pipe[IO, String, Box] =
    _.collect(Box.fromString)

  val parseLines: Pipe[IO, Byte, String] =
    utf8.decode andThen lines

  def getBridge(
      edges: List[Edge],
      size: Int,
      circuits: SortedMap[Box, Circuit] = SortedMap.empty
  ): Option[Edge] =
    edges match
      case edge :: next =>
        val updated =
          (circuits.get(edge.first), circuits.get(edge.second)) match
            case (Some(fst), Some(snd)) if fst != snd =>
              val circuit = fst |+| snd
              circuits ++ circuit.toChain
                .map(box => box -> circuit)
                .toNem
                .toSortedMap
            case (Some(fst), Some(snd)) =>
              circuits
            case (Some(fst), None) =>
              val circuit = fst |+| Circuit.one(edge.second)
              circuits ++ circuit.toChain
                .map(box => box -> circuit)
                .toNem
                .toSortedMap
            case (None, Some(snd)) =>
              val circuit = Circuit.one(edge.first) |+| snd
              circuits ++ circuit.toChain
                .map(box => box -> circuit)
                .toNem
                .toSortedMap
            case (None, None) =>
              val circuit = Circuit.one(edge.first) |+| Circuit.one(edge.second)
              circuits ++ circuit.toChain
                .map(box => box -> circuit)
                .toNem
                .toSortedMap

        val values = updated.values
        val head = values.head
        if (values.size == size && values.forall(_ == head))
          Some(edge)
        else
          getBridge(next, size, updated)
      case Nil =>
        None

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

        getBridge(sorted.toList, boxes.size) match
          case Some(edge) => (edge.first.x * edge.second.x).toString()
          case None       => ""
      }
