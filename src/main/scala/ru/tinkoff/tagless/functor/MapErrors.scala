package ru.tinkoff.tagless.functor
import cats.{ApplicativeError, MonadError}
import org.http4s.{HttpRoutes, Response}


object HandleErrors {
  def apply[F[_]: MonadError[?[_], Throwable]](httpService: HttpRoutes[F])(f: PartialFunction[Throwable, Response[F]]): HttpRoutes[F] =
    httpService
}
