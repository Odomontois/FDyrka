package ru.tinkoff.tagless.functor
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.apply._
import Raise.syntax._
import ru.tinkoff.tagless.functor.AccountError.{AlreadyCreated, DoesNotExist, InsufficientFunds}

trait Accounts[F[_]] extends Transactional[Accounts[F], F] {
  def get(name: String): F[BigDecimal]
  def modify(name: String, state: BigDecimal): F[BigDecimal]
  def create(name: String): F[Unit]
}

object Accounts {
  def apply[F[_]: Sync: Raise[?[_], AccountError]]: F[Accounts[F]] =
    for (ref <- Ref[F].of(Map[String, BigDecimal]()))
      yield AccountsImpl[F](ref)

  private case class AccountsImpl[F[_]: Sync](ref: Ref[F, Map[String, BigDecimal]]) extends Accounts[F] {
    override def get(name: String): F[BigDecimal] =
      for (map <- ref.get; res <- map.get(name).liftTo(DoesNotExist(name))) yield res
    override def modify(name: String, diff: BigDecimal): F[BigDecimal] =
      for {
        (m, upd) <- ref.access
        cur      <- m.get(name).liftTo(DoesNotExist(name))
        next     = cur + diff
        _        <- InsufficientFunds(name, cur, diff).raise.whenA(next < 0)
        _        <- upd(m + (name -> next))
      } yield next
    override def create(name: String): F[Unit] =
      for {
        (m, upd) <- ref.access
        _        <- AlreadyCreated(name).raise.whenA(m.contains(name))
        _        <- upd(m + (name -> BigDecimal(0)))
      } yield ()

    override def begin: F[(Accounts[F], F[Boolean])] =
      for ((m, upd) <- ref.access) yield (AccountsImpl(ref), ref.get >>= upd)
  }
}

trait AccountError extends Throwable

object AccountError {
  final case class DoesNotExist(id: String) extends Exception(s"account $id does not exist") with AccountError
  final case class InsufficientFunds(id: String, has: BigDecimal, diff: BigDecimal)
      extends Exception(s"insufficient funds for account $id to change by $diff: has $has ") with AccountError
  final case class AlreadyCreated(id: String) extends Exception(s"account $id already exists") with AccountError
}
