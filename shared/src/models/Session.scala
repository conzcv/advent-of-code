package shared.models

import ciris._

final case class Session(cookie: String)

object Session:
  def fromEnv: ConfigValue[Effect, Session] =
    for cookie <- env("SESSION_COOKIE")
    yield Session(cookie)
