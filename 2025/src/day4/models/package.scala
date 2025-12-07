package `2025`.day4

import cats.Representable
import cats.Functor
import cats.Traverse
import cats.Applicative
import cats.Eval
import cats.syntax.traverse._

package object models {
  opaque type Dimensions = (Int, Int)

  object Dimensions:
    extension (w: Dimensions)
      def width: Int = w._1
      def height: Int = w._2

  opaque type Coordinates = (Int, Int)

  object Coordinates:
    val zero: Coordinates = (0, 0)

    def of(x: Int, y: Int): Coordinates = (x, y)

    extension (c: Coordinates)
      def x: Int = c._1
      def y: Int = c._2
      def left: Coordinates = (c._1 - 1, c._2)
      def right: Coordinates = (c._1 + 1, c._2)
      def top: Coordinates = (c._1, c._2 - 1)
      def bottom: Coordinates = (c._1, c._2 + 1)

    def projection(using
        D: Dimensions
    ): PartialFunction[(Int, Int), Coordinates] =
      inline def xCondition(x: Int): Boolean = x >= 0 && x < D._1
      inline def yCondition(y: Int): Boolean = y >= 0 && y < D._2
      locally { case (x, y) if xCondition(x) && yCondition(y) => (x, y) }

  opaque type Grid[+A] = Vector[Vector[A]]

  object Grid:

    extension [A](r: Grid[A])
      def toVector: Vector[Vector[A]] = r
      def dimensions: Dimensions = (r.head.length, r.length)
      def map[B](f: A => B): Grid[B] = r.map(_.map(f))
      def apply(coordinates: Coordinates): A = r(coordinates._2)(coordinates._1)

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

    given representable(using
        D: Dimensions
    ): Representable.Aux[Grid, Coordinates] =
      new Representable[Grid]:
        type Representation = Coordinates
        val F: Functor[Grid] = summon

        def index[A](f: Grid[A]): Coordinates => A = f.apply
        def tabulate[A](f: Coordinates => A): Grid[A] =
          (0 until D._2).toVector.map(y =>
            (0 until D._1).toVector.map(x => f(x, y))
          )
}
