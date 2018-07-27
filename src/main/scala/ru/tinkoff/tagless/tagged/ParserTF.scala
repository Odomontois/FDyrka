package ru.tinkoff.tagless
package tagged

import cats.instances.list._
import cats.kernel.Monoid
import Monoid.empty
import Operate.{change, create, transact}
import Raise.raise
import cats.syntax.monoid._

import scala.annotation.tailrec
import scala.util.Try

object ParserTF {
  val Create = "create (\\w+)".r
  val Change = "change (\\w+) ([-+]?[\\d\\.]+)".r
  val Begin  = "begin"
  val End    = "end"
  val Empty  = "\\s*".r

  final case class ParseSub[A](parsed: A, rest: Option[List[String]])

  def parse[A: Operate: Monoid: Raise[?, String]](strings: List[String]): A =
    parseIter[A](strings, empty[A]) match {
      case ParseSub(_, Some(rest)) => raise("unexpected end of transaction")
      case ParseSub(parsed, None)  => parsed
    }

  @tailrec
  def parseIter[A: Operate: Monoid: Raise[?, String]](strings: List[String], parsed: A): ParseSub[A] =
    strings match {
      case Nil => ParseSub(parsed, None)
      case line :: rest =>
        line match {
          case End        => ParseSub(parsed, Some(rest))
          case Create(id) => parseIter(rest, parsed |+| create[A](id))
          case Change(id, count) =>
            val res = Try(BigDecimal(count)) match {
              case util.Failure(err)  => raise(err.toString)
              case util.Success(diff) => change[A](id, diff)
            }
            parseIter(rest, parsed |+| res)
          case Begin =>
            parseSub[A](rest, empty[A]) match {
              case ParseSub(_, None)            => ParseSub(raise("unexpected end of input inside transaction"), Some(rest))
              case ParseSub(sub, Some(remains)) => parseIter(remains, parsed |+| transact(sub))
            }
          case Empty() => parseIter(rest, parsed)
          case _       => ParseSub(parsed |+| raise(s"could not parse: $line"), Some(rest))
        }
    }

  def parseSub[A: Operate: Monoid: Raise[?, String]](strings: List[String], parsed: A): ParseSub[A] =
    parseIter(strings, parsed)

}
