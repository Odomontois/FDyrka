package org.manatki.fdyrka

import cats.arrow.FunctionK

object functionK {
  def apply[F[_]]: MkFunctionK[F] = new MkFunctionK

  final class MkFunctionK[F[_]](val dummy: Boolean = true) extends AnyVal {
    type T

    def apply[G[_]](f: F[T] => G[T]): FunctionK[F, G] = new FunctionK[F, G] {
      def apply[A](fa: F[A]): G[A] = f(fa.asInstanceOf[F[T]]).asInstanceOf[G[A]]
    }
  }
}
