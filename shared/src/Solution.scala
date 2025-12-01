package shared

trait Solution[F[_]]:
  def of(task: Task[F]): F[String]
