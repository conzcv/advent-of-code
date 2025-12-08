package `2025`.day8.models

import cats.kernel.Semigroup
import cats.data.NonEmptyChain

opaque type Circuit = NonEmptyChain[Box]

object Circuit:
  def one(box: Box): Circuit = NonEmptyChain.one(box)

  given Semigroup[Circuit] = summon

  extension (c: Circuit)
    def size: Long = c.length
    def toChain: NonEmptyChain[Box] = c
