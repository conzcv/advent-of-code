package `2025`.day8.models

import cats.kernel.Order

opaque type Edge = (Box, Box, Double)
object Edge:
  def create(a: Box, b: Box): Edge =
    (Order[Box].min(a, b), Order[Box].max(a, b), a.distance(b))

  extension (edge: Edge)
    def first: Box = edge._1
    def second: Box = edge._2
    def associated(other: Edge): Boolean =
      edge._1 == other._1 ||
        edge._1 == other._2 ||
        edge._2 == other._1 ||
        edge._2 == other._2

  given Ordering[Edge] = Ordering[Double].on(_._3)
