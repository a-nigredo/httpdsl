package dev.nigredo.program

import cats.{Id, ~>}
import dev.nigredo.Model.Response
import dev.nigredo.algebra._
import dev.nigredo.compiler.Literal
import dev.nigredo.compiler.model._
import dev.nigredo.program.CheckResponseHeaderSpec.{AssertionHeaderProgram, ResponseHeaderProgram}
import org.specs2.mutable.Specification

class CheckResponseHeaderSpec extends Specification {
  "Check response header" should {
    "pass" in {
      val assertion = FieldAssertion("field1", Gt, Literal(1))
      checkResponseHeader(Response("", 200, Map("field1" -> "2")))(CheckResponseHeader(assertion))
        .foldMap(AssertionHeaderProgram or ResponseHeaderProgram) mustEqual "success"
    }
  }
}

object CheckResponseHeaderSpec {

  object ResponseHeaderProgram extends (ResponseHeader ~> Id) {
    override def apply[A](fa: ResponseHeader[A]) = fa match {
      case ExtractResponseHeaders(response) => response.headers
      case ExtractResponseHeaderValue(headers, fieldName) => headers(fieldName)
    }
  }


  object AssertionHeaderProgram extends (dev.nigredo.algebra.Assertion ~> Id) {
    override def apply[A](fa: dev.nigredo.algebra.Assertion[A]) = fa match {
      case IsTrue(Expr(op, value1, Literal.Int(value2))) => genCompare(op)(value1.toInt)(value2)
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

}