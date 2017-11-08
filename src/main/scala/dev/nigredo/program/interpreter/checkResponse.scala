package dev.nigredo.program.interpreter

import dev.nigredo.Model.Response
import dev.nigredo.compiler.IR._

object checkResponse extends (Seq[Check] => Response) {
  //
  //  implicit val responseSemigroup = new Semigroup[Response] {
  //    override def combine(x: Response, y: Response): Response = x
  //  }
  //
  //  @tailrec
  //  private def fold(data: Seq[Validation])(validated: Validation): Validation = {
  //    if (data.isEmpty) validated
  //    else fold(data.tail)(validated.combine(data.head))
  //  }
  //
  //  override def apply(check: Seq[Check]): Response => Validation = response => {
  //    val res = check.map {
  //      //    case Check(ResponseBody, assertion) =>
  //      case Check(ResponseHeader, assertion) => assert(assertion)(response)
  //      case _ => throw new RuntimeException("")
  //    }
  //    fold(res.tail)(res.head)
  //  }
  //
  //  private def assert(assertion: Assertion)(response: Response): Validated[Vector[String], Response] =
  //    assertion match {
  //      case FieldAssertion(name, op, NumberLiteral(litValue)) =>
  //        checkResponseHeader[Int](response)(name)(litValue)(_.toInt)(genCompare(op))
  //      case FieldAssertion(name, op, BooleanLiteral(litValue)) =>
  //        checkResponseHeader[Boolean](response)(name)(litValue)(_.toBoolean)(genCompare(op))
  //      case FieldAssertion(name, op, StringLiteral(litValue)) =>
  //        checkResponseHeader[String](response)(name)(litValue)(identity)(genCompare(op))
  //      case And(ls, rs) =>
  //        assert(ls)(response).andThen(assert(rs))
  //      case Or(ls, rs) =>
  //        val leftResult = assert(ls)(response)
  //        if (leftResult.isInvalid) leftResult
  //        else leftResult.andThen(assert(rs))
  //    }
  //
//    private def genCompare[A](op: Operation)(lOp: A)(rOp: A)(implicit ordering: Ordering[A]): Boolean =
//      op match {
//        case IR.Eq | IR.Sw | IR.Ew | IR.Consist => ordering.equiv(lOp, rOp)
//        case IR.Neq => !ordering.equiv(lOp, rOp)
//        case IR.Gt => ordering.gt(lOp, rOp)
//        case IR.Gte => ordering.gteq(lOp, rOp)
//        case IR.Lt => ordering.lt(lOp, rOp)
//        case IR.Lte => ordering.lt(lOp, rOp)
//      }
  //
  //  private def checkResponseHeader[A](response: Response)(name: String)(compareThat: A)(castTo: String => A)(comparePredicate: A => A => Boolean) = {
  //    (for {
  //      headerValue <- extractValue(response.headers.get(name))(s"Header '$name' was not found in a response")
  //      compareWith <- cast[A](headerValue)(castTo)(s"Could not convert header value '$name:$headerValue' to Number")
  //      result <- compare[Response](comparePredicate(compareWith)(compareThat))(response)(s"Value '$compareWith' for header '$name' is not equal to '$compareThat'")
  //    } yield result).toValidated
  //  }
  //
  //  private def extractValue(data: Option[String])(err: String) =
  //    Either.fromOption(data, Vector(err))
  //
  //  private def cast[A](data: String)(caster: String => A)(err: String) =
  //    Either.catchNonFatal(caster(data)).leftMap(_ => Vector(err))
  //
  //  private def compare[A](compareWith: => Boolean)(success: A)(error: String) =
  //    Either.cond(compareWith, success, Vector(error))
  override def apply(v1: Seq[Check]) = ???
}
