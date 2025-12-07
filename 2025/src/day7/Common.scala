package `2025`.day7

import `2025`.day4.models.Grid
import `2025`.day7.models.Location
import cats.effect.IO
import cats.syntax.option.*
import fs2.Pipe
import fs2.text.*
import shared.Task
import `2025`.day4.models.Dimensions
import `2025`.day4.models.GridStore
import `2025`.day7.models.Neighborhood

object Common:
  private val parseLines: Pipe[IO, Byte, Vector[Location]] =
    _.through(utf8.decode andThen lines)
      .filter(_.nonEmpty)
      .map(_.toVector.collect(Location.fromChar))

  def extractGrid(task: Task[IO]): IO[Grid[Location]] =
    for
      positions <- task.input.through(parseLines).compile.toVector
      gridOption = Grid.from(positions)
      grid <- gridOption.liftTo[IO](new Exception("grid building error"))
    yield grid

  import Location._
  import Neighborhood._

  private def propagation(using Dimensions): GridStore[Location] => Location =
    store =>
      val neighborhood: Neighborhood[Location] =
        store.experiment(Neighborhood.ofCoordinates)

      neighborhood match
        case Right(Splitter(_), Beam(i), _)       => Splitter(Some(i))
        case Right(_, StartPoint, _)              => Beam(1)
        case Right(_, Beam(i), Splitter(Some(j))) => Beam(i + j)
        case Right(_, _, Splitter(Some(j)))       => Beam(j)
        case Right(_, Beam(i), _)                 => Beam(i)

        case Left(Splitter(_), Beam(i), _)       => Splitter(Some(i))
        case Left(_, StartPoint, _)              => Beam(1)
        case Left(_, Beam(i), Splitter(Some(j))) => Beam(i + j)
        case Left(_, _, Splitter(Some(j)))       => Beam(j)
        case Left(_, Beam(i), _)                 => Beam(i)

        case Full(Splitter(_), Beam(i), _, _) => Splitter(Some(i))
        case Full(_, StartPoint, _, _)        => Beam(1)
        case Full(_, Beam(i), Splitter(Some(j)), Splitter(Some(k))) =>
          Beam(i + j + k)
        case Full(_, Beam(i), Splitter(Some(j)), _)           => Beam(i + j)
        case Full(_, Beam(i), _, Splitter(Some(j)))           => Beam(i + j)
        case Full(_, _, Splitter(Some(i)), Splitter(Some(j))) => Beam(i + j)
        case Full(_, Beam(i), _, _)                           => Beam(i)
        case Full(_, _, Splitter(Some(i)), _)                 => Beam(i)
        case Full(_, _, _, Splitter(Some(i)))                 => Beam(i)
        case neighborhood => neighborhood.focus

  def propagate(
      store: GridStore[Location],
      prev: Option[GridStore[Location]] = None
  )(using Dimensions): GridStore[Location] =
    val step = store.coflatMap(propagation)
    val finished = prev.contains(step) // not the best exit condition
    if (finished) step else propagate(step, Some(store))
