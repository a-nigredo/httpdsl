package dev.nigredo.algebra

import cats.free.Free
import cats.free.Free.liftF
import dev.nigredo.Model.Response
import dev.nigredo.algebra.CheckResponse.Result
import dev.nigredo.compiler.IR.Operation

sealed trait CheckResponse[A]

final case class ExtractResponseBody(response: Response) extends CheckResponse[String]

final case class ExtractResponseHeaders(response: Response) extends CheckResponse[Map[String, String]]

final case class ExtractResponseHeaderValue(headers: Map[String, String], fieldName: String) extends CheckResponse[String]

final case class CastToInt(value: String) extends CheckResponse[Int]

final case class CastToBoolean(value: String) extends CheckResponse[Boolean]

sealed trait Compare[A] extends CheckResponse[A]

final case class CompareInt(operation: Operation, value1: Int, value2: Int) extends Compare[Boolean]

final case class CompareString(operation: Operation, value1: String, value2: String) extends Compare[Boolean]

final case class CompareBoolean(operation: Operation, value1: Boolean, value2: Boolean) extends Compare[Boolean]

final case class CompareAnd[A](lc: Compare[A], rc: CompareAnd[A]) extends Compare[Boolean]

final case class CompareOr[A](lc: Compare[A], rc: CompareAnd[A]) extends Compare[Boolean]

final case class CompareNot[A](c: Compare[A]) extends CheckResponse[Boolean]

final case class CompareResult(boolean: Boolean) extends Compare[String]

object CompareResult {
  def apply(boolean: Boolean): Result[String] =
    liftF[CheckResponse, String](new CompareResult(boolean))

}

object ExtractResponseBody {
  def apply(response: Response): Result[String] =
    liftF[CheckResponse, String](new ExtractResponseBody(response))
}

object ExtractResponseHeaders {
  def apply(response: Response): Result[Map[String, String]] =
    liftF[CheckResponse, Map[String, String]](new ExtractResponseHeaders(response))
}

object CastToInt {
  def apply(value: String): Result[Int] =
    liftF[CheckResponse, Int](new CastToInt(value))
}

object CastToBoolean {
  def apply(value: String): Result[Boolean] =
    liftF[CheckResponse, Boolean](new CastToBoolean(value))
}

object CompareInt {
  def apply(operation: Operation, value1: Int, value2: Int): Result[Boolean] =
    liftF[CheckResponse, Boolean](new CompareInt(operation, value1, value2))
}

object CompareAnd {
  def apply[A](lc: Compare[A], rc: CompareAnd[A]): Result[Boolean] =
    liftF[CheckResponse, Boolean](new CompareAnd(lc, rc))
}

object ExtractResponseHeaderValue {
  def apply(headers: Map[String, String], fieldName: String): Result[String] =
    liftF[CheckResponse, String](new ExtractResponseHeaderValue(headers, fieldName))
}

object CheckResponse {
  type Result[A] = Free[CheckResponse, A]
}
