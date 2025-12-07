package `2025`.day4

import `2025`.day4.models.Grid
import `2025`.day4.models.Neighborhood
import `2025`.day4.models.Dimensions

import cats.Representable
import cats.data.RepresentableStore
import cats.effect.Concurrent
import cats.effect.IO
import shared.Solution
import shared.Task

import cats.syntax.foldable._

import models.*

final class Part1 extends Solution[IO]:
  def numberOfNeighbors(using
      D: Dimensions
  ): GridStore[Position] => Option[Int] =
    store =>
      val focusNeighborhood: Neighborhood[Position] =
        store.experiment(Neighborhood.ofCoordinates)

      focusNeighborhood.focus match
        case Position.Empty    => None
        case Position.Occupied => Some(focusNeighborhood.foldFrontier(_.count))

  def of(task: Task[IO]): IO[String] =
    Common.extractGrid(task).map { grid =>
      given Dimensions = grid.dimensions

      val store: GridStore[Position] =
        RepresentableStore(grid, Coordinates.zero)

      store
        .coflatMap(numberOfNeighbors)
        .fa
        .toIterable
        .count(_.exists(_ < 4))
        .toString()
    }
