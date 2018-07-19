package org.manatki.fdyrka

import cats.data.EitherT
import cats.effect._
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monad._
import cats.{MonadError, ~>}
import mainecoon.syntax.functorK._

final case class DiApp[F[_]: MonadError[?[_], Throwable]](
    input: Input[F],
    parser: Parser[F],
    output: Output[F],
) {
  def run: F[Unit] =
    output.putLine("DI app started") >>
      (input.readLine >>= parser.parseAndRun)
        .handleErrorWith(ex => output.putLine(s"error: $ex") as true)
        .iterateWhile(identity) >>
      output.putLine("DI app exiting")
}

object DiApp extends IOApp {
  def unliftErr[F[_], E](f: E => Throwable)(implicit err: MonadError[F, Throwable]): EitherT[F, E, ?] ~> F =
    functionK[EitherT[F, E, ?]](_.leftMap(f).value.flatMap(_.raiseOrPure[F]))

  def makeApp[F[_]: Concurrent]: F[DiApp[F]] =
    for {
      storage <- Storage[F, String, BigDecimal]
      account = AccountService(storage.mapK(EitherT.liftK[F, AccountError]), 100)
        .mapK(unliftErr(AppAccountError))
      output = Output.console[F]
      input  = Input.console[F]
      exec   = ExecuteCommand.account[F](account, output)
      parser = Parser(exec.mapK(EitherT.liftK[F, ParseError]))
        .mapK(unliftErr(AppParseError))
    } yield DiApp(input, parser, output)

  override def run(args: List[String]): IO[ExitCode] =
    makeApp[IO] flatMap (_.run) as ExitCode.Success
}

sealed abstract class AppError(message: String) extends Exception(message, null, false, true)

final case class AppParseError(err: ParseError)     extends AppError(s"Parse Error: $err")
final case class AppAccountError(err: AccountError) extends AppError(s"Account Error: $err")
