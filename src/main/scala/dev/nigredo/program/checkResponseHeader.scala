package dev.nigredo.program

import cats.free.Free
import dev.nigredo.Model.Response
import dev.nigredo.algebra.ResponseHeader.Program
import dev.nigredo.algebra.{AssertionOps, CompareValue, ResponseHeaderOps}
import dev.nigredo.compiler.IR._

object checkResponseHeader extends (Response => CheckResponseHeader => Free[Program, String]) {

  override def apply(response: Response): CheckResponseHeader => Free[Program, String] = check => {
    val I = implicitly[AssertionOps[Program]]
    val D = implicitly[ResponseHeaderOps[Program]]
    import I._, D._

    def assert(assertion: Assertion)(headers: Map[String, String]): Free[Program, Boolean] = {

      def logic(lop: Assertion)(rop: Assertion)(p: Boolean => Boolean => Boolean) = {
        for {
          a1 <- assert(lop)(headers)
          a2 <- assert(rop)(headers)
        } yield p(a1)(a2)
      }

      def fieldAssertion(lit: Literal)(op: Operation)(name: String) = {
        for {
          headerValue <- extractResponseHeaderValue(headers)(name)
          result <- lit match {
            case NumberLiteral(v) => asInt(CompareValue[Int](op, headerValue, v))
            case BooleanLiteral(v) => asBoolean(CompareValue[Boolean](op, headerValue, v))
            case StringLiteral(v) => asString(CompareValue[String](op, headerValue, v))
          }
        } yield result
      }

      assertion match {
        case FieldAssertion(name, op, lit) => fieldAssertion(lit)(op)(name)
        case And(lop, rop) => logic(lop)(rop)(x => y => x && y)
        case Or(lop, rop) => logic(lop)(rop)(x => y => x || y)
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
