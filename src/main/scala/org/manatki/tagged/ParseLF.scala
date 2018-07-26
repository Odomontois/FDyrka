package org.manatki.tagged

import cats.kernel.Monoid

import scala.annotation.tailrec
import scala.util.Try
import cats.syntax.monoid._
import cats.instances.list._

import Monoid.empty
import Operational.{change, create, transact}

object ParseLF {
  val Create = "create (\\w+)".r
  val Change = "change (\\w+) ([-+]?[\\d\\.]+)".r
  val Begin  = "begin"
  val End    = "end"
  val Empty  = "\\s*".r

  final case class ParseSub[A](parsed: A, rest: Option[List[String]])

  def parse[A: Operational: Monoid](strings: List[String]): Either[String, A] =
    parseIter[A](strings, empty[A]) flatMap {
      case ParseSub(_, Some(rest)) => Left("unexpected end of transaction")
      case ParseSub(parsed, None)  => Right(parsed)
    }

  @tailrec
  def parseIter[A: Operational: Monoid](strings: List[String], parsed: A): Either[String, ParseSub[A]] =
    strings match {
      case Nil => Right(ParseSub(parsed, None))
      case line :: rest =>
        line match {
          case End        => Right(ParseSub(parsed, Some(rest)))
          case Create(id) => parseIter(rest, parsed |+| create[A](id))
          case Change(id, count) =>
            Try(BigDecimal(count)) match {
              case util.Failure(err)  => Left(err.toString)
              case util.Success(diff) => parseIter(rest, parsed |+| change[A](id, diff))
            }
          case Begin =>
            parseSub[List[A]](rest, Nil) match {
              case Left(err)                => Left(err)
              case Right(ParseSub(_, None)) => Left("unexpected end of input inside transaction")
              case Right(ParseSub(sub, Some(remains))) =>
                parseIter(remains, parsed |+| transact(sub))
            }
          case Empty() => parseIter(rest, parsed)
          case _       => Left(s"could not parse: $line")
        }
    }

  def parseSub[A: Operational: Monoid](strings: List[String], parsed: A): Either[String, ParseSub[A]] =
    parseIter(strings, parsed)

  implicit def liftOperationalToList[A](implicit op: Operational[A]): Operational[List[A]] =
    new Operational[List[A]] {
      override def create(id: String): List[A]                   = List(op.create(id))
      override def change(id: String, diff: BigDecimal): List[A] = List(op.change(id, diff))
      override def transact(xs: List[List[A]]): List[A]          = xs.flatten
    }

}
