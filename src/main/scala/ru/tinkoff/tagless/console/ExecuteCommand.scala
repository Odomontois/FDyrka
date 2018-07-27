package ru.tinkoff.tagless.console

import cats.FlatMap
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import mainecoon.autoFunctorK

@autoFunctorK
trait ExecuteCommand[F[_]] {
  def create(id: String): F[Unit]
  def transfer(from: String, to: String)(amount: BigDecimal): F[Unit]
  def get(id: String): F[Unit]
  def help(str: String): F[Unit]
}

object ExecuteCommand {
  def account[F[_]: FlatMap](svc: AccountService[F], output: Output[F]): ExecuteCommand[F] =
    new ExecuteCommand[F] {
      override def create(id: String): F[Unit] = svc.create(Account(id))
      override def transfer(from: String, to: String)(amount: BigDecimal): F[Unit] =
        svc.transfer(Account(from), Account(to), Amount(amount))
      override def get(id: String): F[Unit] =
        svc.read(Account(id)).map(_.value.toString) >>= output.putLine
      override def help(str: String): F[Unit] = output.putLine(str)
    }
}
