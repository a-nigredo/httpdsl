package dev.nigredo.program

import cats.free.Free
import dev.nigredo.{ResponseModel, algebra}
import dev.nigredo.algebra.Response.Program
import dev.nigredo.algebra.{AssertionOps, ResponseOps}
import dev.nigredo.compiler.Literal
import dev.nigredo.compiler.model._

object checkResponseHeader extends (ResponseModel => CheckResponseHeader => Free[Program, String]) {

  override def apply(response: ResponseModel): CheckResponseHeader => Free[Program, String] = checkValue => {
    val I = implicitly[AssertionOps[Program]]
    val D = implicitly[ResponseOps[Program]]
    import I._, D._

    def check(assertion: Assertion): Free[Program, Boolean] = {

      def logic(lop: Assertion)(rop: Assertion)(p: Boolean => Boolean => Boolean) =
        for {
          a1 <- check(lop)
          a2 <- check(rop)
        } yield p(a1)(a2)

      def fieldAssertion(lit: Literal)(op: Operation)(name: String) =
        for {
          headerValue <- extractHeader(response)(name)
          result <- assert(algebra.Expr(op, headerValue, lit))
        } yield result

      assertion match {
        case FieldAssertion(name, op, lit) => fieldAssertion(lit)(op)(name)
        case And(lop, rop) => logic(lop)(rop)(x => y => x && y)
        case Or(lop, rop) => logic(lop)(rop)(x => y => x || y)
        case Not(data) => check(data).map(!_)
      }
    }

    for {
      assertResult <- check(checkValue.assertion)
      result <- result(assertResult)
    } yield result
  }
}
