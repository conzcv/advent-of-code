package `2025`.day4

import `2025`.day4.models.Grid
import `2025`.day4.models.Neighborhood
import cats.Representable
import cats.data.RepresentableStore
import cats.effect.Concurrent
import cats.effect.IO
import shared.Solution
import shared.Task

import cats.syntax.foldable._

import models.*

final class Part1 extends Solution[IO]:
  val countOfNeighbors: GridStore[Position] => Option[Int] =
    store =>
      val neighborhood: Coordinates => Neighborhood[Coordinates] =
        Coordinates.neighborhood(store.fa.width, store.fa.height)

      val focusNeighborhood: Neighborhood[Position] =
        store.experiment(neighborhood)

      focusNeighborhood.focus match
        case Position.Empty    => None
        case Position.Occupied => Some(focusNeighborhood.foldFrontier(_.count))

  def of(task: Task[IO]): IO[String] =
    Common.extractGrid(task).map { grid =>
      given Representable.Aux[Grid, Coordinates] =
        Grid.representable(grid.width, grid.height)

      val store: GridStore[Position] =
        RepresentableStore(grid, Coordinates.zero)

      store
        .coflatMap(countOfNeighbors)
        .fa
        .toIterable
        .count(_.exists(_ < 4))
        .toString()
    }
