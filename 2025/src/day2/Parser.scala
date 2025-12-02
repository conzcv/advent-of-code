package `2025`.day2

import fs2._
import fs2.text._
import `2025`.day2.models.IdRange

object Parser:
  private def parseTokens[F[_]](
      prev: String,
      stream: Stream[F, String]
  ): Pull[F, String, Unit] =
    stream.pull.uncons1.flatMap {
      case Some(string, tail) =>
        val tokens: Chunk[String] = Chunk.array((prev ++ string).split(","))
        val (first, last) = tokens.splitAt(tokens.size - 1)
        first.traverse {
          case nonEmpty if nonEmpty.nonEmpty =>
            Pull.output1(nonEmpty)
          case _ =>
            Pull.pure(())
        } >> parseTokens(last.head.get, tail)
      case None =>
        if (prev.isEmpty()) Pull.done else Pull.output1(prev)
    }

  def tokens[F[_]]: Pipe[F, Byte, String] =
    bytes => parseTokens("", bytes.through(utf8.decode)).stream.map(_.trim())

  val RangePattern = "(\\d+)-(\\d+)".r

  val range: PartialFunction[String, IdRange] =
    case RangePattern(fst, snd) => IdRange(fst, snd)
