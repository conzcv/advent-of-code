package `2025`

import shared.AOCApplication
import shared.models.Identifier
import `2025`.day1.Day1
import `2025`.day2.Day2
import `2025`.day3.Day3
import cats.effect.IO
import shared.Solution

val solutions: Map[Identifier, Solution[IO]] = Map(
  Identifier(2025, 1, 1) -> Day1.Part1[IO],
  Identifier(2025, 1, 2) -> Day1.Part2[IO],
  Identifier(2025, 2, 1) -> Day2.Part1[IO],
  Identifier(2025, 2, 2) -> Day2.Part2[IO],
  Identifier(2025, 3, 1) -> Day3.CommonSolution[IO](2),
  Identifier(2025, 3, 2) -> Day3.CommonSolution[IO](12)
)

object Main2025 extends AOCApplication(solutions)
