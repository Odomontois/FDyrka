package org.manatki.fdyrka

import cats.arrow.FunctionK
import cats.data.EitherT
import cats.effect._
import cats.{ApplicativeError, Monad, MonadError, ~>}
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.monad._
import cats.syntax.functor._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.applicativeError._
import mainecoon.syntax.functorK._

final case class DiApp[F[_]: MonadError[?[_], Throwable]](
    input: Input[F],
    parser: Parser[F],
    output: Output[F],
) {
  def run: F[Unit] =
    (input.readLine >>= parser.parseAndRun)
      .handleErrorWith(ex => output.putLine(s"error: $ex") as true)
      .iterateWhile(identity)
      .void
}

object DiApp extends IOApp {
  def unliftErr[F[_], E, E1](f: E => E1)(implicit err: MonadError[F, E1]): EitherT[F, E, ?] ~> F =
    functionK[EitherT[F, E, ?]](_.leftMap(f).value.flatMap(_.raiseOrPure[F]))

  def runApp[F[_]: Concurrent]: F[Unit] =
    for {
      storage <- Storage[F, String, BigDecimal]
      account = AccountService(storage.mapK(EitherT.liftK[F, AccountError]), 100)
        .mapK(unliftErr(err => AppAccountError(err): Throwable))
      input  = Input.console[F]
      output = Output.console[F]
      exec   = ExecuteCommand.account[F](account, output)
      parser = Parser(exec.mapK(EitherT.liftK[F, ParseError]))
        .mapK(unliftErr(err => AppParseError(err): Throwable))
      _  <- output.putLine("DI app started")
      di = DiApp(input, parser, output)
      _  <- di.run
      _  <- output.putLine("DI app exiting")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = runApp[IO] as ExitCode.Success
}

sealed abstract class AppError(message: String) extends Exception(message, null, false, true)

final case class AppParseError(err: ParseError)     extends AppError(s"Parse Error: $err")
final case class AppAccountError(err: AccountError) extends AppError(s"Account Error: $err")
