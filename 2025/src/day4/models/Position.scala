package `2025`.day4.models

enum Position:
  case Empty
  case Occupied
  def count: Int = this match
    case Empty    => 0
    case Occupied => 1

object Position:
  val fromChar: PartialFunction[Char, Position] =
    case '@' => Position.Occupied
    case '.' => Position.Empty
