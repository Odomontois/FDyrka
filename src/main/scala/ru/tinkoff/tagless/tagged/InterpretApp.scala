package ru.tinkoff.tagless.tagged

import cats.data.OptionT
import cats.effect.{ExitCode, IO, IOApp}
import fs2.{io, text}
import Interpreter.implicits._
import cats.implicits._
import ru.tinkoff.tagless.tagged.Interpreter.Transaction

object InterpretApp extends IOApp {
  val Resource = "/operations.list"
  val src =
    OptionT(IO(Option(getClass.getResourceAsStream(Resource))))
      .getOrElseF(IO.raiseError(new Exception(s"$Resource not found")))

  val lines =
    io.readInputStream(src, 100)
      .through(text.utf8Decode)
      .through(text.lines)
      .compile
      .toList

  override def run(args: List[String]): IO[ExitCode] =
    for {
      ls                       <- lines
      result                   = ParserTF.parse[Interpreter.Result[Unit]](ls)
      Transaction(acc, errors) = result.runS(Transaction()).value
      _                        <- errors.traverse_(s => IO(println(s)))
      _                        <- IO(println(s"final state:  $acc"))
    } yield ExitCode.Success
}
