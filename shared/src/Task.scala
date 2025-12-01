package shared

import fs2.Stream

final case class Task[F[_]](input: Stream[F, Byte])
