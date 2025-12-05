package `2025`.day4

import cats.Representable
import cats.Functor
import cats.Traverse
import cats.Applicative
import cats.Eval
import cats.syntax.traverse._

package object models {
  opaque type Coordinates = (Int, Int)

  object Coordinates:
    val zero: Coordinates = (0, 0)

    def neighborhood(
        width: Int,
        height: Int
    ): Coordinates => Neighborhood[Coordinates] =
      def of(x: Int, y: Int): Option[Coordinates] =
        for
          _x <- Option.when(x >= 0 && x < width)(x)
          _y <- Option.when(y >= 0 && y < height)(y)
        yield (_x, _y)

      locally { case (x, y) =>
        Neighborhood(
          focus = (x, y),
          topleft = of(x - 1, y - 1),
          top = of(x, y - 1),
          topright = of(x + 1, y - 1),
          right = of(x + 1, y),
          bottomright = of(x + 1, y + 1),
          bottom = of(x, y + 1),
          bottomleft = of(x - 1, y + 1),
          left = of(x - 1, y)
        )
      }

  opaque type Grid[+A] = Vector[Vector[A]]

  extension [A](r: Grid[A])
    def toVector: Vector[Vector[A]] = r
    def width: Int = r.head.length
    def height: Int = r.length
    def map[B](f: A => B): Grid[B] = r.map(_.map(f))
    def apply(coordinates: Coordinates): A = r(coordinates._2)(coordinates._1)

  object Grid:
    def from[A](vector: Vector[Vector[A]]): Option[Grid[A]] =
      for
        head <- vector.headOption
        lengthsAreEqual = vector.forall(_.length == head.length)
        result <- Option.when(head.length > 0 && lengthsAreEqual)(vector)
      yield result

    given Functor[Grid] =
      new:
        def map[A, B](fa: Grid[A])(f: A => B): Grid[B] = fa.map(f)

    given Traverse[Grid] =
      new Traverse[Grid]:
        def traverse[G[_]: Applicative, A, B](fa: Grid[A])(
            f: A => G[B]
        ): G[Grid[B]] =
          fa.toVector.traverse(_.traverse(f))
        def foldLeft[A, B](fa: Grid[A], b: B)(f: (B, A) => B): B =
          fa.toVector.foldLeft(b)((accumulator, vector) =>
            vector.foldLeft(accumulator)(f)
          )
        def foldRight[A, B](fa: Grid[A], lb: Eval[B])(
            f: (A, Eval[B]) => Eval[B]
        ): Eval[B] =
          fa.toVector.foldRight(lb)((vector, accumulator) =>
            vector.foldRight(accumulator)(f)
          )
        override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] = fa.map(f)

    def representable(
        width: Int,
        height: Int
    ): Representable.Aux[Grid, Coordinates] =
      new Representable[Grid]:
        type Representation = Coordinates
        val F: Functor[Grid] = summon

        def index[A](f: Grid[A]): Coordinates => A = f.apply
        def tabulate[A](f: Coordinates => A): Grid[A] =
          (0 until height).toVector.map(y =>
            (0 until width).toVector.map(x => f(x, y))
          )
}
