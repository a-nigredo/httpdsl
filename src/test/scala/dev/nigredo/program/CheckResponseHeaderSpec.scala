package dev.nigredo.program

import cats.{Id, ~>}
import dev.nigredo.Model.Response
import dev.nigredo.algebra._
import dev.nigredo.algebra.ResponseHeader.Program
import dev.nigredo.compiler.Literal
import dev.nigredo.compiler.model._
import org.specs2.mutable.Specification

class CheckResponseHeaderSpec extends Specification {
  "Test" should {
    "test" in {
      val resp = Response("", 203, Map("field1" -> "1"))
      val assertion = dev.nigredo.compiler.model.Or(FieldAssertion("field1", Gt, Literal.Int(2)), FieldAssertion("field1", Gt, Literal.Int(1)))
      val cr = CheckResponseHeader(assertion)

      object Rh extends (ResponseHeader ~> Id) {
        override def apply[A](fa: ResponseHeader[A]) = fa match {
          case ExtractResponseHeaders(response) => response.headers
          case ExtractResponseHeaderValue(headers, fieldName) => headers(fieldName)
        }
      }
      object Ass extends (dev.nigredo.algebra.Assertion ~> Id) {
        override def apply[A](fa: dev.nigredo.algebra.Assertion[A]) = fa match {
          case IsTrue(Expr(_, value1, Literal.Int(value2))) => value1.toInt == value2
          case Result(true) => "success"
          case Result(false) => "error"
        }

        private def genCompare[A](op: Operation)(lOp: A)(rOp: A)(implicit ordering: Ordering[A]): Boolean =
          op match {
            case Eq | Sw | Ew | Consist => ordering.equiv(lOp, rOp)
            case Neq => !ordering.equiv(lOp, rOp)
            case Gt => ordering.gt(lOp, rOp)
            case Gte => ordering.gteq(lOp, rOp)
            case Lt => ordering.lt(lOp, rOp)
            case Lte => ordering.lteq(lOp, rOp)
          }
      }
      val int: Program ~> Id = Ass or Rh
      val r = checkResponseHeader(resp)(cr).foldMap(int)
      r mustEqual "success"
    }
  }
}
