package `2025`.day8.models

import cats.kernel.Order

opaque type Box = (Int, Int, Int)

object Box:
  given Order[Box] = summon
  given Ordering[Box] = summon

  extension (a: Box)
    def x: Int = a._1
    def y: Int = a._2
    def z: Int = a._3
    def distance(b: Box): Double =
      math.sqrt(
        math.pow(a._1 - b._1, 2) +
          math.pow(a._2 - b._2, 2) +
          math.pow(a._3 - b._3, 2)
      )

  val BoxPattern = "(\\d+),(\\d+),(\\d+)".r

  val fromString: PartialFunction[String, Box] =
    case BoxPattern(fst, snd, thrd) =>
      (fst.toInt, snd.toInt, thrd.toInt)
