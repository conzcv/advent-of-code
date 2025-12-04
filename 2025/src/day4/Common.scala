package `2025`.day4

import cats.effect.IO
import cats.syntax.option.*
import fs2.Pipe
import fs2.text.*
import shared.Task

import models.*

object Common:
  val parseLines: Pipe[IO, Byte, Vector[Position]] =
    _.through(utf8.decode andThen lines)
      .filter(_.nonEmpty)
      .map(_.toVector.collect(Position.fromChar))

  def extractGrid(task: Task[IO]): IO[Grid[Position]] =
    for
      positions <- task.input.through(parseLines).compile.toVector
      gridOption = Grid.from(positions)
      grid <- gridOption.liftTo[IO](new Exception("grid building error"))
    yield grid
