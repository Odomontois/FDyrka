package org.manatki.fdyrka

import cats.MonadError
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monadError._
import cats.syntax.option._
import mainecoon.autoFunctorK

@autoFunctorK
trait AccountService[F[_]] {
  def read(acc: Account): F[Amount]
  def create(acc: Account): F[Unit]
  def transfer(from: Account, to: Account, amount: Amount): F[Unit]
}

object AccountService {

  def apply[F[_]: MonadError[?[_], AccountError]](storage: Storage[String, BigDecimal, F], initialAmount: BigDecimal = 0): AccountService[F] =
    Impl(storage, initialAmount)

  private case class Impl[F[_]](storage: Storage[String, BigDecimal, F], initialAmount: BigDecimal)(implicit F: MonadError[F, AccountError])
      extends AccountService[F] {
    import AccountError._

    override def read(acc: Account): F[Amount] =
      for {
        res <- storage.get(acc.id)
        amt <- res.liftTo[F](UnknownAccount(acc): AccountError)
      } yield Amount(amt)

    override def create(acc: Account): F[Unit] =
      storage
        .transact(_(acc.id).fold(true -> List(acc.id -> initialAmount))(_ => false -> List.empty))
        .ensure(AlreadyExists)(identity)
        .void

    override def transfer(from: Account, to: Account, amount: Amount): F[Unit] =
      storage.transact { get =>
        val results = for {
          f <- get(from.id).toRight(UnknownAccount(from)).ensure(Deficit)(_ >= amount.value)
          t <- get(to.id).toRight(UnknownAccount(to))
        } yield List(from.id -> (f - amount.value), to.id -> (t + amount.value))

        results.left.toOption -> results.foldK
      }.flatMap(_.traverse_(_.raiseError[F, Unit]))
  }
}

sealed trait AccountError extends Product with Serializable

object AccountError {
  final case class UnknownAccount(acc: Account) extends AccountError
  case object Deficit                           extends AccountError
  case object AlreadyExists                     extends AccountError
}

final case class Account(id: String)
final case class Amount(value: BigDecimal)
