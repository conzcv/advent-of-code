package `2025`

import `2025`.day1.Day1
import `2025`.day2.Day2
import `2025`.day3.Day3
import `2025`.day4
import `2025`.day5
import `2025`.day6
import `2025`.day7
import cats.effect.IO
import shared.AOCApplication
import shared.Solution
import shared.models.Identifier

val solutions: Map[Identifier, Solution[IO]] = Map(
  Identifier(2025, 1, 1) -> new Day1.Part1,
  Identifier(2025, 1, 2) -> new Day1.Part2,
  Identifier(2025, 2, 1) -> new Day2.Part1,
  Identifier(2025, 2, 2) -> new Day2.Part2,
  Identifier(2025, 3, 1) -> Day3.CommonSolution(2),
  Identifier(2025, 3, 2) -> Day3.CommonSolution(12),
  Identifier(2025, 4, 1) -> new day4.Part1,
  Identifier(2025, 4, 2) -> new day4.Part2,
  Identifier(2025, 5, 1) -> new day5.Part1,
  Identifier(2025, 5, 2) -> new day5.Part2,
  Identifier(2025, 6, 1) -> new day6.Part1,
  Identifier(2025, 6, 2) -> new day6.Part2,
  Identifier(2025, 7, 1) -> new day7.Part1,
  Identifier(2025, 7, 2) -> new day7.Part2
)

object Main2025 extends AOCApplication(solutions)
