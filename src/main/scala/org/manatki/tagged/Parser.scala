package org.manatki.tagged
import scala.annotation.tailrec
import scala.util.Try

import Operation.{Change => change, Create => create, Transact => transact}

object Parser {
  val Create = "create (\\w+)".r
  val Change = "change (\\w+) ([-+]?[\\d\\.]+)".r
  val Begin  = "begin"
  val End    = "end"
  val Empty  = "\\s*".r

  final case class ParseSub(parsed: Vector[Operation], rest: Option[List[String]])

  def parse(strings: List[String]): Either[String, Vector[Operation]] =
    parseIter(strings, Vector()) flatMap {
      case ParseSub(_, Some(rest)) => Left("unexpected end of transaction")
      case ParseSub(parsed, None)  => Right(parsed)
    }

  @tailrec
  def parseIter(strings: List[String], parsed: Vector[Operation]): Either[String, ParseSub] =
    strings match {
      case Nil => Right(ParseSub(parsed, None))
      case line :: rest =>
        line match {
          case End        => Right(ParseSub(parsed.reverse, Some(rest)))
          case Create(id) => parseIter(rest, parsed :+ create(id))
          case Change(id, count) =>
            Try(BigDecimal(count)) match {
              case util.Failure(err)  => Left(err.toString)
              case util.Success(diff) => parseIter(rest, parsed :+ change(id, diff))
            }
          case Begin =>
            parseSub(rest, Vector()) match {
              case l @ Left(_)              => l
              case Right(ParseSub(_, None)) => Left("unexpected end of input inside transaction")
              case Right(ParseSub(sub, Some(remains))) =>
                parseIter(remains, parsed :+ transact(sub.toList))
            }
          case Empty() => parseIter(rest, parsed)
          case _       => Left(s"could not parse: $line")
        }
    }

  def parseSub(strings: List[String], parsed: Vector[Operation]): Either[String, ParseSub] = parseIter(strings, parsed)
}
