package dev.nigredo.program

import cats.free.Free
import dev.nigredo.Model.Response
import dev.nigredo.algebra.{AssertionOps, CompareValue, ResponseHeader, ResponseHeaderOps}
import dev.nigredo.compiler.IR._

object CheckResponseHeader {

  def program(response: Response)(check: CheckResponseHeader)(implicit I: AssertionOps[ResponseHeader],
                                                              D: ResponseHeaderOps[ResponseHeader]): Free[ResponseHeader, String] = {
    import D._
    import I._

    def assert(assertion: Assertion)(headers: Map[String, String]): Free[ResponseHeader, Boolean] = {
      //TODO Remove duplication
      assertion match {
        case FieldAssertion(name, op, NumberLiteral(v)) =>
          for {
            headerValue <- extractResponseHeaderValue(headers)(name)
            result <- asInt(CompareValue[Int](op, headerValue, v))
          } yield result
        case FieldAssertion(name, op, BooleanLiteral(v)) =>
          for {
            headerValue <- extractResponseHeaderValue(headers)(name)
            result <- asBoolean(CompareValue[Boolean](op, headerValue, v))
          } yield result
        case FieldAssertion(name, op, StringLiteral(v)) =>
          for {
            headerValue <- extractResponseHeaderValue(headers)(name)
            result <- asString(CompareValue[String](op, headerValue, v))
          } yield result
        case And(lop, rop) =>
          for {
            a1 <- assert(lop)(headers)
            a2 <- assert(lop)(headers)
          } yield a1 && a2
        case Or(lop, rop) => or(lop)(rop)
        case Not(data) => assert(data)(headers).map(!_)
      }
    }

    for {
      headers <- extractResponseHeaders(response)
      assertResult <- assert(check.assertion)(headers)
      result <- result(assertResult)
    } yield result
  }

}
