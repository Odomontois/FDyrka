package org.manatki.fdyrka

import cats.MonadError
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import mainecoon.autoFunctorK

import scala.util.Try

@autoFunctorK
trait Parser[F[_]] {
  def parseAndRun(s: String): F[Boolean]
}

object Parser {
  import ParseError.NumberFormatError

  val Create   = "create (\\w+)".r
  val Transfer = "transfer (\\w+) (\\w+) (\\d+)".r
  val Get      = "get (\\w+)".r
  val Exit     = "exit"
  val Help     = "help"

  def apply[F[_]: MonadError[?[_], ParseError]](exec: ExecuteCommand[F]): Parser[F] = {
    case Create(name) => exec.create(name) as true
    case Transfer(from, to, countStr) =>
      (Try(BigDecimal(countStr)).toEither
        .leftMap(ex => NumberFormatError(ex.getMessage): ParseError)
        .raiseOrPure[F] >>=
        exec.transfer(from, to)) as true

    case Get(name) => exec.get(name) as true
    case Exit      => false.pure[F]
    case Help =>
      exec help
        """available commands:
          |create <name> : new account
          |transfer <name> <name> <amount> : transfer money
          |get <name> : get account current balance
          |exit : exit application
          |help : print current help""".stripMargin as true
  }
}

sealed trait ParseError extends Product with Serializable

object ParseError {
  final case class BadFormat(s: String)         extends ParseError
  final case class NumberFormatError(s: String) extends ParseError
}
