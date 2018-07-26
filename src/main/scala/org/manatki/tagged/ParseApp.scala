package org.manatki.tagged

import cats.data.OptionT
import cats.effect.{ExitCode, IO, IOApp}
import fs2.{io, text}

object ParseApp extends IOApp {
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
      ls <- lines
      _  <- IO(println(Parser.parse(ls)))
    } yield ExitCode.Success
}
