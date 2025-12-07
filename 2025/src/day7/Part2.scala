package `2025`.day7

import shared.Solution
import shared.Task
import cats.effect.IO
import `2025`.day4.models.Dimensions
import `2025`.day4.models.GridStore
import `2025`.day7.models.Location
import `2025`.day4.models.Coordinates
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.data.RepresentableStore

final class Part2 extends Solution[IO]:
  def of(task: Task[IO]): IO[String] =
    Common.extractGrid(task).map { grid =>
      given Dimensions = grid.dimensions

      val store: GridStore[Location] =
        RepresentableStore(grid, Coordinates.zero)

      Common
        .propagate(store)
        .fa
        .toVector
        .last
        .foldMap {
          case Location.Beam(intensity) => intensity
          case _                        => 0
        }
        .toString()
    }
