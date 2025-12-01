package `2025`.day1.models

import cats.kernel.Monoid

sealed trait Rotation:
  def angle: Int
  def turns: Int

object Rotation:
  private object members:
    final case class Right(angle: Int, turns: Int) extends Rotation
    final case class Left(angle: Int, turns: Int) extends Rotation
    final case class Empty(turns: Int) extends Rotation:
      def angle: Int = 0

  val RightPattern = "R(\\d+)".r
  val LeftPattern = "L(\\d+)".r

  def fromString(string: String): Rotation =
    string match
      case RightPattern(digits) =>
        val int = digits.toInt
        members.Right(int % 100, int / 100)
      case LeftPattern(digits) =>
        val int = digits.toInt
        members.Left(int % 100, int / 100)
      case _ =>
        members.Empty(0)

  import members._

  given Monoid[Rotation] =
    new:
      def combine(x: Rotation, y: Rotation): Rotation =
        (x, y) match
          case (Empty(t1), Empty(t2))         => Empty(t1 + t2)
          case (Empty(t1), Right(a2, t2))     => Right(a2, t1 + t2)
          case (Empty(t1), Left(a2, t2))      => Left(a2, t1 + t2)
          case (Right(a1, t1), Empty(t2))     => Right(a1, t1 + t2)
          case (Right(a1, t1), Right(a2, t2)) =>
            val angle = (a1 + a2) % 100
            val turns = t1 + t2 + (a1 + a2) / 100
            if (angle == 0) Empty(turns) else Right(angle, turns)
          case (Right(a1, t1), Left(a2, t2)) if a1 < a2 =>
            Left(a2 - a1, t1 + t2 + 1)
          case (Right(a1, t1), Left(a2, t2)) if a1 > a2 =>
            Right(a1 - a2, t1 + t2)
          case (Right(a1, t1), Left(a2, t2)) =>
            Empty(t1 + t2 + 1)
          case (Left(a1, t1), Empty(t2))    => Left(a1, t1 + t2)
          case (Left(a1, t1), Left(a2, t2)) =>
            val angle = (a1 + a2) % 100
            val turns = t1 + t2 + (a1 + a2) / 100
            if (angle == 0) Empty(turns) else Left(angle, turns)
          case (Left(a1, t1), Right(a2, t2)) if a1 < a2 =>
            Right(a2 - a1, t1 + t2 + 1)
          case (Left(a1, t1), Right(a2, t2)) if a1 > a2 =>
            Left(a1 - a2, t1 + t2)
          case (Left(a1, t1), Right(a2, t2)) =>
            Empty(t1 + t2 + 1)
      def empty: Rotation = members.Empty(0)
