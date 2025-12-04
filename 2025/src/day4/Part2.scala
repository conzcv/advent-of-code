package `2025`.day4

import cats.Representable
import cats.data.RepresentableStore
import cats.effect.IO
import shared.Solution
import shared.Task

import models.*

final class Part2 extends Solution[IO]:
  type Result[A] = (value: A, changed: Boolean)

  val take: GridStore[Position] => Result[Position] =
    store =>
      val neighborhood: Coordinates => Neighborhood[Coordinates] =
        Coordinates.neighborhood(store.fa.width, store.fa.height)

      val focusNeighborhood: Neighborhood[Position] =
        store.experiment(neighborhood)

      focusNeighborhood.focus match
        case Position.Empty    => (Position.Empty, false)
        case Position.Occupied =>
          val count: Int = focusNeighborhood.foldFrontier(_.count)
          if (count < 4) (Position.Empty, true) else (Position.Occupied, false)

  def takeToTheLimit(store: GridStore[Position], count: Int = 0): Int =
    val taken = store.coflatMap(take)
    val changes: Int = taken.fa.toVector.flatten.count(_.changed)
    if (changes > 0) takeToTheLimit(taken.map(_.value), count + changes)
    else count

  def of(task: Task[IO]): IO[String] =
    Common.extractGrid(task).map { grid =>
      given Representable.Aux[Grid, Coordinates] =
        Grid.representable(grid.width, grid.height)

      val store: GridStore[Position] =
        RepresentableStore(grid, Coordinates.zero)

      takeToTheLimit(store).toString()
    }
