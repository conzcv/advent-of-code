package shared.models

final case class Identifier(year: Int, day: Int, part: Int):
  override def toString: String = s"$day/$year, part $part"

object Identifier:
  def apply(year: Int, day: Int): Identifier = Identifier(year, day, 1)
