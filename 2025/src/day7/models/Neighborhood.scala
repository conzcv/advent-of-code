package `2025`.day7.models

import cats.Functor
import `2025`.day4.models.Dimensions
import `2025`.day4.models.Coordinates

sealed trait Neighborhood[+A]:
  def focus: A

object Neighborhood:
  final case class TopLeft[+A](focus: A, right: A) extends Neighborhood[A]
  final case class Top[+A](focus: A, left: A, right: A) extends Neighborhood[A]
  final case class TopRight[+A](focus: A, left: A) extends Neighborhood[A]
  final case class Right[+A](focus: A, top: A, left: A) extends Neighborhood[A]
  final case class Left[+A](focus: A, top: A, right: A) extends Neighborhood[A]
  final case class Full[+A](focus: A, top: A, left: A, right: A)
      extends Neighborhood[A]

  given Functor[Neighborhood] =
    new:
      def map[A, B](fa: Neighborhood[A])(f: A => B): Neighborhood[B] = fa match
        case TopLeft(focus, right)         => TopLeft(f(focus), f(right))
        case Top(focus, left, right)       => Top(f(focus), f(left), f(right))
        case TopRight(focus, left)         => TopRight(f(focus), f(left))
        case Right(focus, top, left)       => Right(f(focus), f(top), f(left))
        case Left(focus, top, right)       => Left(f(focus), f(top), f(right))
        case Full(focus, top, left, right) =>
          Full(f(focus), f(top), f(left), f(right))

  def ofCoordinates(
      c: Coordinates
  )(using D: Dimensions): Neighborhood[Coordinates] =
    if (c.x == 0 && c.y == 0)
      TopLeft(c, c.right)
    else if (c.x == D.width - 1 && c.y == 0)
      TopRight(c, c.left)
    else if (c.y == 0)
      Top(c, c.left, c.right)
    else if (c.x == 0)
      Left(c, c.top, c.right)
    else if (c.x == D.width - 1)
      Right(c, c.top, c.left)
    else
      Full(c, c.top, c.left, c.right)
