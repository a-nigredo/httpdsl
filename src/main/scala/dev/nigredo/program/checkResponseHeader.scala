package dev.nigredo.program

import cats.free.Free
import dev.nigredo.Model.Response
import dev.nigredo.algebra
import dev.nigredo.algebra.ResponseHeader.Program
import dev.nigredo.algebra.{AssertionOps, ResponseHeaderOps}
import dev.nigredo.compiler.Literal
import dev.nigredo.compiler.model._

object checkResponseHeader extends (Response => CheckResponseHeader => Free[Program, String]) {

  override def apply(response: Response): CheckResponseHeader => Free[Program, String] = checkValue => {
    val I = implicitly[AssertionOps[Program]]
    val D = implicitly[ResponseHeaderOps[Program]]
    import D._
    import I._

    def check(assertion: Assertion)(headers: Map[String, String]): Free[Program, Boolean] = {

      def logic(lop: Assertion)(rop: Assertion)(p: Boolean => Boolean => Boolean) = {
        for {
          a1 <- check(lop)(headers)
          a2 <- check(rop)(headers)
        } yield p(a1)(a2)
      }

      def fieldAssertion(lit: Literal)(op: Operation)(name: String) = {
        for {
          headerValue <- extractResponseHeaderValue(headers)(name)
          result <- assert(algebra.Expr(op, headerValue, lit))
        } yield result
      }

      assertion match {
        case FieldAssertion(name, op, lit) => fieldAssertion(lit)(op)(name)
        case And(lop, rop) => logic(lop)(rop)(x => y => x && y)
        case Or(lop, rop) => logic(lop)(rop)(x => y => x || y)
        case Not(data) => check(data)(headers).map(!_)
      }
    }

    for {
      headers <- extractResponseHeaders(response)
      assertResult <- check(checkValue.assertion)(headers)
      result <- result(assertResult)
    } yield result
  }
}
