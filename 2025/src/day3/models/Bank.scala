package `2025`.day3.models

final case class Bank(batteries: Vector[Short]):
  def joltage: Short = ???

object Bank:
  val DigitsPattern = "(\\d+)".r
  val fromString: PartialFunction[String, Bank] =
    case DigitsPattern(digits) => Bank(digits.toVector.map(_.asDigit.toShort))
