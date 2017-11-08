package dev.nigredo.program

import cats.implicits._
import cats.~>
import dev.nigredo.Model.Response
import dev.nigredo.algebra._
import dev.nigredo.compiler.IR._
import org.specs2.mutable.Specification

class CheckResponseHeaderProgramSpecs extends Specification {
  "asdsa" should {
    "sd" in {
      val response = Response("", 200, Map("field1" -> "1"))
      val check = Check(ResponseHeader, And(FieldAssertion("field1", Gt, NumberLiteral(1)), And(FieldAssertion("field1", Gt, NumberLiteral(1)), FieldAssertion("field1", Gt, NumberLiteral(1)))))

      type Res[A] = Either[String, A]

      val res = checkResponseHeaderProgram(response)(check).foldMap {
        new (CheckResponse ~> Res) {
          override def apply[A](fa: CheckResponse[A]) = fa match {
            case ExtractResponseHeaders(r) => r.headers.asRight
            case ExtractResponseHeaderValue(headers, fieldName) => Either.fromOption(headers.get(fieldName), "Err")
            case CastToInt(v) => Either.catchNonFatal(v.toInt).leftMap(_ => "cast err")
            case CompareInt(op, v1, v2) => Either.cond(v1 == v2, true, "error")
            case CompareResult(true) => "good".asRight
            case CompareResult(false) => "bad".asRight
          }
        }
      }
      println(res)
      ok
    }
  }
}
