package `2025`.day7.models

enum Location:
  case Empty
  case Splitter(i: Option[Long])
  case Beam(i: Long)
  case StartPoint

  def intensity: Long = this match
    case Empty       => 0
    case Splitter(i) => i.getOrElse(0L)
    case Beam(i)     => i
    case StartPoint  => 1

object Location:
  val fromChar: PartialFunction[Char, Location] =
    case '^' => Location.Splitter(None)
    case 'S' => Location.StartPoint
    case '.' => Location.Empty
