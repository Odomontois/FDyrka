package ru.tinkoff.tagless.functor

import cats.{Applicative, ApplicativeError}

trait Raise[F[_], E] {
  def raise[A](err: E): F[A]
}

object Raise {
  implicit def raiseApplicativeError[F[_], E, E1](implicit appErr: ApplicativeError[F, E], sub: E1 <:< E): Raise[F, E1] =
    new Raise[F, E1] {
      override def raise[A](err: E1): F[A] = appErr.raiseError(err)
    }

  object syntax{
    final implicit class RaiseOps[E](val err: E) extends AnyVal{
      def raise[F[_], A](implicit raise: Raise[F, E]): F[A] = raise.raise(err)
    }

    final implicit class RaiseOptionOps[A](val opt: Option[A]) extends AnyVal {
      def liftTo[F[_]] = new RaiseLiftToApplied[F, A](opt)
    }
  }

  class RaiseLiftToApplied[F[_], A](val opt: Option[A]) extends AnyVal {
    def apply[E](err: => E)(implicit raise: Raise[F, E], app: Applicative[F]): F[A] =
      opt match {
        case None    => raise.raise(err)
        case Some(a) => app.pure(a)
      }
  }
}