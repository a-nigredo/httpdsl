package dev.nigredo.compiler

import fastparse.all._
import fastparse.{all, core}
import model._

object parser {

  final val Body = "body"
  final val Header = "header"
  final val OrToken = "or"
  final val AndToken = "and"
  final val multiValueSeparator = ','
  final val NL = " \r\n"

  val alphaNumeric = ('0' to 'z').filter(_.isLetterOrDigit)
  val multiValue = NL.+:(',')
  private val space: all.Parser[Unit] = P(CharsWhileIn(" \r\n")).?

  val boolean: core.Parser[Literal, Char, String] = P("true" | "false").!.map(x => Literal(x.toBoolean))
  val digits: core.Parser[Literal, Char, String] = P(CharsWhileIn('0' to '9')).!.map(x => Literal(x.toInt))
  val string: core.Parser[Literal, Char, String] =
    P("\"" ~/ CharsWhile(NamedFunction(!"\"\\".contains(_: Char), "StringChars")).rep.! ~/ "\"").map(Literal.apply)
  val array: all.Parser[Literal] =
    P("[" ~/ (boolean | digits | string | array)
      .rep(sep = CharPred(x => NL.contains(x) || x == ',').rep)
      .map(x => Literal(x)) ~ "]")

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T): V = f(t)

    override def toString(): String = name
  }

  val header: core.Parser[(String, String), Char, String] = {
    val separator = Seq('(', ')', '<', '>', '@', ',', ';', ':', '"', '/', '[', ']', '?', '=', '{', '}', 32.toChar, 9.toChar)
    val CTL = (0 to 31).+:(127.toChar)
    P(
      CharsWhileIn((0 to 127)
        .map(_.toChar)
        .filter(x => !separator.contains(x) && !CTL.contains(x))).! ~/ ":" ~/ CharPred(x => !multiValue.contains(x)).rep.!
    )
  }

  val headers = P(header.rep(min = 1, sep = multiValueSeparator.toString))

  val assertion: all.Parser[(String, Operation, Literal)] =
    P(P(CharIn(alphaNumeric).rep(1) ~/ ".".?).rep(1).!
      ~/ space
      ~/ P("eq" | "neq" | ("gt" ~/ "e".?) | ("lt" ~/ "e".?) | "consist" | "sw" | "ew").!.map(Operation.apply)
      ~/ space
      ~/ (digits | string | array | boolean))

  val assertions: core.Parser[Assertion, Char, String] =
    P(assertionsTerm ~/ (OrToken ~/ assertionsTerm).rep).map { x =>
      x._2.foldLeft[Assertion](x._1) {
        case (zero, v) => Or(zero, v)
      }
    }

  private lazy val assertionsTerm: core.Parser[Assertion, Char, String] =
    P((assertionsNotFactor ~/ (AndToken ~/ assertionsNotFactor).rep).map { x =>
      x._2.foldLeft[Assertion](x._1) {
        case (zero, v) => And(zero, v)
      }
    })

  private lazy val assertionsNotFactor: core.Parser[Assertion, Char, String] =
    P(space ~/ "not".!.? ~/ space ~/ assertionsFactor).map {
      case (Some(_), asrt) => Not(asrt)
      case (None, asrt) => asrt
    }

  private lazy val assertionsFactor: core.Parser[Assertion, Char, String] =
    P(space ~ (assertion.map(FieldAssertion.apply) | "(" ~ assertions ~ ")") ~ space)

  val check: core.Parser[Check, Char, String] =
    P("check".?
      ~ space
      ~ "response"
      ~ space
      ~/ (Body | Header).!.opaque("Assertion type is either missed or wrong. Possible values: body | header")
      ~/ space ~/ assertions
    ).map { x =>
      x._1 match {
        case Body => CheckResponseBody(x._2)
        case Header => CheckResponseHeader(x._2)
      }
    }

  val program: core.Parser[Seq[AST], Char, String] =
    P("Send"
      ~ space
      ~ P("get" | "put" | "post" | "delete").!.map(Method.apply)
      ~ space
      ~ "request"
      ~ space
      ~ "to"
      ~ space
      ~/ P("http" ~/ "s".? ~/ "://" ~/ CharsWhileIn(('0' to 'z').++(Seq('/', '&'))).rep ~/ P("?" ~/ CharsWhileIn('0' to 'z')).?).!
      ~ space
      ~ ("with" ~/ space ~/ "headers" ~/ space ~/ headers ~ space).?
      ~ space
      ~ "and".?
      ~ space
      ~ check.rep)
      .rep(min = 1, sep = ".")
      .map(_.map(x => (Request(x._2, x._1, x._3.map(_.toMap).getOrElse(Map.empty)), x._4)))
}
