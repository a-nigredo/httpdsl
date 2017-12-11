package dev.nigredo.compiler

import dev.nigredo.compiler.model._
import dev.nigredo.compiler.parser._
import fastparse.core.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class ParserSpec
  extends Specification
    with ScalaCheck {

  import ParserSpec._

  implicit def assertArbitrary: Arbitrary[(String, (String, Operation, Literal))] = Arbitrary(assertGen)

  implicit def assertsBodyArbitrary: Arbitrary[(String, Assertion)] = Arbitrary(assertionsGen)

  def assertFailure[A](value: Parsed[A, Char, String]): MatchResult[Any] =
    value match {
      case Parsed.Success(v, _) => ko(s"Expected failure but success $v")
      case Parsed.Failure(_, _, _) => ok
    }

  def assertSuccess[A](actual: Parsed[A, Char, String], expected: A): MatchResult[Any] =
    actual match {
      case Parsed.Success(v, _) => v mustEqual expected
      case Parsed.Failure(wrong, _, _) => ko(s"Expected success but failure: $wrong")
    }

  "Parser" should {
    "parse header" in {
      assertSuccess(header.parse("Content-type:app/json"), ("Content-type", "app/json"))
      assertSuccess(header.parse("Content:app"), ("Content", "app"))
    }
    "parse headers" in {
      assertSuccess(headers.parse("Content-type:app/json,Header1:v1"), Seq(("Content-type", "app/json"), ("Header1", "v1")))
    }
    "parse boolean literal" in {
      assertSuccess(boolean.parse("true"), Literal(true))
      assertSuccess(boolean.parse("false"), Literal(false))
    }
    "parse number literal" in prop { data: Int =>
      assertSuccess(digits.parse(data.toString), Literal(data))
    }.setGen(Gen.choose(1, Int.MaxValue))
    "parse string literal" in prop { data: String =>
      assertSuccess(string.parse(s""""$data""""), Literal(data))
    }.setGen(Gen.alphaNumStr)
    "parse array literal" in {
      assertSuccess(array.parse("[true, false]"), Literal(Seq(Literal(true), Literal(false))))
      assertSuccess(array.parse("[1, 2]"), Literal(Seq(Literal(1), Literal(2))))
      assertSuccess(array.parse("""["str1", "str2"]"""), Literal(Seq(Literal("str1"), Literal("str2"))))
      assertSuccess(array.parse("""[[1, 2]]"""), Literal(Seq(Literal(Seq(Literal(1), Literal(2))))))
      assertSuccess(array.parse("""[[1,2],[3,5]]"""), Literal(Seq(Literal(Seq(Literal(1), Literal(2))), Literal(Seq(Literal(3), Literal(5))))))
    }
    "parse assertion statement" in prop { data: (String, (String, Operation, Literal)) =>
      val (value, expected) = data
      assertSuccess(assertion.parse(value), expected)
    }
    "not parse assertion statement if field is not set" in {
      assertFailure(assertion.parse(" lt 1"))
    }
    "not parse assertion statement if operation is not set" in {
      assertFailure(assertion.parse("field 1"))
    }
    "not parse assertion statement if assertion value is not set" in {
      assertFailure(assertion.parse("field lt"))
    }
    "not parse assertion statement if assertion without spaces" in {
      assertFailure(assertion.parse("fieldlt1"))
    }
    "not parse assertion statement if assertion operation is wrong" in {
      assertFailure(assertion.parse("field wrongOp test"))
    }
    "not parse assertion statement if it is empty" in {
      assertFailure(assertion.parse(""))
    }
    "parse assertions" in prop { data: (String, Assertion) =>
      assertSuccess(assertions.parse(data._1), data._2)
    }
    "parse check response body" in {
      assertSuccess(parser.check.parse("check response body field1 lt 4"), CheckResponseBody(FieldAssertion("field1", Lt, Literal(4))))
    }
    "parse check response header" in {
      assertSuccess(parser.check.parse("check response header field1 lt 4"), CheckResponseHeader(FieldAssertion("field1", Lt, Literal(4))))
    }
    "not parse check if target is wrong" in {
      assertFailure(parser.check.parse("check response field1 lt 4"))
    }
    "parse send request with headers and check" in {
      val expr = "Send get request to http://localhost:8080 with headers Content-type:application/json,Accept:test and check response body field1.field2 lt 10"
      val expected = Seq((Request("http://localhost:8080", Get, Map("Content-type" -> "application/json", "Accept" -> "test")),
        Seq(CheckResponseBody(FieldAssertion("field1.field2", Lt, Literal(10))))))
      assertSuccess(program.parse(expr), expected)
    }
    "parse send request without headers but check" in {
      val expr = "Send get request to http://localhost:8080 and check response body field1.field2 lt 10"
      val expected = Seq((Request("http://localhost:8080", Get, Map.empty),
        Seq(CheckResponseBody(FieldAssertion("field1.field2", Lt, Literal(10))))))
      assertSuccess(program.parse(expr), expected)
    }
    "parse send request without headers and check" in {
      assertSuccess(program.parse("Send get request to http://localhost:8080"), Seq((Request("http://localhost:8080", Get, Map.empty), Nil)))
    }
  }

}

object ParserSpec {

  val assertionsGen: Gen[(String, Assertion)] = {

    val f1 = ("""field1 gt "f1"""", FieldAssertion("field1", Gt, Literal("f1")))
    val f2 = ("""field2 lt "f2"""", FieldAssertion("field2", Lt, Literal("f2")))

    def and(lOp: String, rOp: String) = s"$lOp and $rOp"

    def or(lOp: String, rOp: String) = s"$lOp or $rOp"

    Gen.oneOf(Vector(
      f1, f2,
      (and(f1._1, f2._1), And(f1._2, f2._2)),
      (or(f1._1, f2._1), Or(f1._2, f2._2)),
      (s"(${and(f1._1, f2._1)}) and ${f1._1}", And(And(f1._2, f2._2), f1._2)),
      (s"${f1._1} and (${and(f1._1, f2._1)})", And(f1._2, And(f1._2, f2._2))),
      (s"(${and(f1._1, f2._1)}) or ${f1._1}", Or(And(f1._2, f2._2), f1._2)),
      (s"${f1._1} or (${and(f1._1, f2._1)})", Or(f1._2, And(f1._2, f2._2))),
      (s"${f1._1} or (${and(f1._1, s"(${or(f1._1, f2._1)})")})", Or(f1._2, And(f1._2, Or(f1._2, f2._2)))),
      (s"(${and(f1._1, s"(${or(f1._1, f2._1)})")}) or ${f1._1}", Or(And(f1._2, Or(f1._2, f2._2)), f1._2))
    ))
  }

  val assertGen: Gen[(String, (String, Operation, Literal))] = for {
    parts <- Gen.choose(1, 10)
    fieldLength <- Gen.choose(1, 7)
    field <- Gen.listOfN(parts, Gen.listOfN(fieldLength, Gen.oneOf(alphaNumeric)).map(_.mkString)).map(_.mkString("."))
    ops <- Gen.oneOf(Operation.ops)
    value <- Gen.listOfN(parts, Gen.listOfN(fieldLength, Gen.oneOf(alphaNumeric)).map(_.mkString)).map(_.mkString)
  } yield (s"""$field $ops "$value"""", (field, Operation(ops), Literal(value)))

}