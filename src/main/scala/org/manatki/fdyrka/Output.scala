package org.manatki.fdyrka

import cats.effect.Sync
import mainecoon.autoFunctorK

@autoFunctorK
trait Output[F[_]] {
  def putLine(s: String): F[Unit]
}

object Output {
  implicit def console[F[_]: Sync]: Output[F] =
    s => Sync[F].delay(println(s))
}
