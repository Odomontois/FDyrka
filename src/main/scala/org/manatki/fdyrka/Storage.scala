package org.manatki.fdyrka

import cats.effect.concurrent.MVar
import cats.effect.syntax.bracket._
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.functor._
import mainecoon.autoFunctorK

@autoFunctorK
trait Storage[K, V, F[_]] {
  def get(id: K): F[Option[V]]
  def transact[A](job: (K => Option[V]) => (A, List[(K, V)])): F[A]
}

object Storage {
  def apply[F[_]: Concurrent, K, V]: F[Storage[K, V, F]] = for (mvar <- MVar.of(Map.empty[K, V])) yield Impl(mvar)

  private case class Impl[F[_], K, V](mvar: MVar[F, Map[K, V]])(implicit F: Sync[F]) extends Storage[K, V, F] {
    override def get(id: K): F[Option[V]] = mvar.read.map(_.get(id))
    override def transact[A](job: (K => Option[V]) => (A, List[(K, V)])): F[A] =
      mvar.take.bracketCase { map =>
        val (a, items) = job(map.lift)
        mvar.put(map ++ items) as a
      } {
        case (_, ExitCase.Completed) => F.unit
        case (old, _)                => mvar.put(old)
      }
  }

}
