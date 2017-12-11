package dev.nigredo.program

import cats.{Id, ~>}
import dev.nigredo.Model.Response
import dev.nigredo.algebra.ResponseHeader.Program
import dev.nigredo.algebra._
import dev.nigredo.compiler.IR
import dev.nigredo.compiler.IR._
import org.specs2.mutable.Specification

class CheckResponseHeaderSpec extends Specification {
  "Test" should {
    "test" in {
      val resp = Response("", 203, Map("field1" -> "1"))
      val assertion = Or(FieldAssertion("field1", Gt, Literal.Int(2)), FieldAssertion("field1", Gt, Literal.Int(1)))
      val cr = IR.CheckResponseHeader(assertion)
      import dev.nigredo.algebra.ResponseHeaderOps._
      import dev.nigredo.algebra.AssertionOps._

      object Rh extends (ResponseHeader ~> Id) {
        override def apply[A](fa: ResponseHeader[A]) = fa match {
          case ExtractResponseHeaders(response) => response.headers
          case ExtractResponseHeaderValue(headers, fieldName) => headers(fieldName)
        }
      }
      object Ass extends (dev.nigredo.algebra.Assertion ~> Id) {
        override def apply[A](fa: dev.nigredo.algebra.Assertion[A]) = fa match {
          case AssertionAsInt(value) => value.value1.toInt == value.value2
          case AssertionResult(true) => "success"
          case AssertionResult(false) => "error"
        }

        private def genCompare[A](op: Operation)(lOp: A)(rOp: A)(implicit ordering: Ordering[A]): Boolean =
          op match {
            case IR.Eq | IR.Sw | IR.Ew | IR.Consist => ordering.equiv(lOp, rOp)
            case IR.Neq => !ordering.equiv(lOp, rOp)
            case IR.Gt => ordering.gt(lOp, rOp)
            case IR.Gte => ordering.gteq(lOp, rOp)
            case IR.Lt => ordering.lt(lOp, rOp)
            case IR.Lte => ordering.lt(lOp, rOp)
          }
      }
      val int: Program ~> Id = Ass or Rh
      val r = checkResponseHeader(resp)(cr).foldMap(int)
      r mustEqual "success"
    }
  }
}
