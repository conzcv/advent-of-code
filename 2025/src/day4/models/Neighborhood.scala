package `2025`.day4.models

import cats.Functor
import cats.kernel.Monoid
import cats.syntax.foldable._
import cats.syntax.semigroup._

final case class Neighborhood[+A](
    focus: A,
    topleft: Option[A],
    top: Option[A],
    topright: Option[A],
    right: Option[A],
    bottomright: Option[A],
    bottom: Option[A],
    bottomleft: Option[A],
    left: Option[A]
):
  def foldFrontier[B: Monoid](f: A => B): B =
    topleft.foldMap(f) |+|
      top.foldMap(f) |+|
      topright.foldMap(f) |+|
      right.foldMap(f) |+|
      bottomright.foldMap(f) |+|
      bottom.foldMap(f) |+|
      bottomleft.foldMap(f) |+|
      left.foldMap(f)

object Neighborhood:
  given Functor[Neighborhood] =
    new:
      def map[A, B](fa: Neighborhood[A])(f: A => B): Neighborhood[B] =
        Neighborhood(
          focus = f(fa.focus),
          topleft = fa.topleft.map(f),
          top = fa.top.map(f),
          topright = fa.topright.map(f),
          right = fa.right.map(f),
          bottomright = fa.bottomright.map(f),
          bottom = fa.bottom.map(f),
          bottomleft = fa.bottomleft.map(f),
          left = fa.left.map(f)
        )
