package org.manatki.tagged

import cats.data.OptionT
import cats.effect.{ExitCode, IO, IOApp}
import fs2.{io, text}
import Interpreter.{accumulatingOperate, raiseString, stateMonoid, accumulate}
import cats.implicits._

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
      ls                     <- lines
      result                 = accumulate(ParserTF.parse[Interpreter.Result[Unit]](ls))
      (state, (messages, _)) = result.value.run.runEmpty.value
      _                      <- messages.traverse_(s => IO(println(s)))
      _                      <- IO(println(s"final state:  $state"))
    } yield ExitCode.Success
}
