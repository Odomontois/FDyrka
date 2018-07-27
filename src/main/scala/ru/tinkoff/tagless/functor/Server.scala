package ru.tinkoff.tagless.functor
import cats.effect.{ConcurrentEffect, IO, Sync}
import fs2.StreamApp
import fs2.StreamApp.ExitCode
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.concurrent.ExecutionContext.Implicits.global

case class Server[F[_]: ConcurrentEffect]() {
  implicit val dsl: Http4sDsl[F] = new Http4sDsl[F] {}

  def init: F[fs2.Stream[F, ExitCode]] =
    for {
      accountsService <- Accounts[F]
      accountsModule  = AccountsModule(accountsService)
      _               <- Sync[F].delay(println("server started and http://localhost:8383"))
    } yield
      BlazeBuilder[F]
        .bindHttp(8383, "localhost")
        .mountService(accountsModule.service, "/accounts")
        .serve

  def serve = fs2.Stream.force(init)

}

object Server extends StreamApp[IO] {
  val server = Server[IO]()

  override def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, StreamApp.ExitCode] = server.serve
}
