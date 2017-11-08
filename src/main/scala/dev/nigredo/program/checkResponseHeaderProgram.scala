package dev.nigredo.program

import cats.free.Free
import dev.nigredo.Model.Response
import dev.nigredo.algebra._
import dev.nigredo.compiler.IR._

object checkResponseHeaderProgram extends (Response => Check => Free[CheckResponse, String]) {

  override def apply(response: Response): Check => Free[CheckResponse, String] = check => {
    for {
      headers <- ExtractResponseHeaders(response)
      r <- assert(check.assertion, headers)
      result <- CompareResult(r)
    } yield result
  }

  def assert(assertion: Assertion, headers: Map[String, String]): Free[CheckResponse, Boolean] = {
    assertion match {
      case FieldAssertion(name, op, NumberLiteral(v)) =>
        for {
          headerValue <- ExtractResponseHeaderValue(headers, name)
          value <- CastToInt(headerValue)
          result <- CompareInt(op, v, value)
        } yield result
      case And(lop, rop) =>
        for {
          v1 <- assert(lop, headers)
          v2 <- assert(rop, headers)
        } yield v1 && v2

      //        case FieldAssertion(_, _, BooleanLiteral(v)) => CastToBoolean(v)
    }
  }
}
