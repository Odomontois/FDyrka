package org.manatki.fdyrka

import cats.effect.Sync
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import mainecoon.autoFunctorK

import scala.io.StdIn

@autoFunctorK
sealed trait Input[F[_]] {
  def readLine: F[String]
}

object Input {
  def console[F[_]: Sync]: Input[F] = new Input[F] {
    override def readLine: F[String] = Sync[F].delay(StdIn.readLine())
  }
}
