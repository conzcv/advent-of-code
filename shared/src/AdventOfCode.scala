package shared

import models.Identifier
import org.http4s.client.Client
import shared.models.Session
import org.http4s._
import org.http4s.implicits._

trait AdventOfCode[F[_]]:
  def get(id: Identifier): Task[F]

object AdventOfCode:
  def apply[F[_]](client: Client[F], session: Session): AdventOfCode[F] =
    new:
      def get(id: Identifier): Task[F] =
        val uri =
          uri"https://adventofcode.com" / id.year.toString / "day" / id.day.toString / "input"
        val req = Request[F](uri = uri).addCookie("session", session.cookie)
        Task(client.stream(req).flatMap(_.body))
