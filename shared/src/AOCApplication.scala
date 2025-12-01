package shared

import models.Identifier
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import cats.syntax.all._
import shared.models.Session
import org.http4s.client.JavaNetClientBuilder

trait AOCApplication(solutions: Map[Identifier, Solution[IO]]) extends IOApp:
  val console = Console[IO]

  final def runSolution(id: Identifier): IO[String] =
    for
      session <- Session.fromEnv.load[IO]
      client = JavaNetClientBuilder[IO].create
      aoc = AdventOfCode(client, session)
      task = aoc.get(id)
      solution <- solutions
        .get(id)
        .liftTo[IO](new Exception(s"unknown id: '$id'"))
      answer <- solution.of(task)
    yield answer

  val identifier: List[String] => Either[String, Identifier] =
    case year :: day :: part :: _ =>
      for
        y <- year.toIntOption.toRight("year should be a number")
        d <- day.toIntOption.toRight("day should be a number")
        p = part.toIntOption.getOrElse(1)
      yield Identifier(y, d, p)
    case year :: day :: _ =>
      for
        y <- year.toIntOption.toRight("year should be a number")
        d <- day.toIntOption.toRight("day should be a number")
      yield Identifier(y, d)
    case _ =>
      Left("Year and day options are required.")

  final def run(args: List[String]): IO[ExitCode] =
    identifier(args) match
      case Right(id) =>
        runSolution(id).attempt.flatMap {
          case Right(answer) =>
            console
              .println(s"Task '$id'. The answer is $answer")
              .as(ExitCode.Success)
          case Left(exception) =>
            console.printStackTrace(exception).as(ExitCode.Error)
        }
      case Left(error) =>
        console.errorln(error).as(ExitCode.Error)
