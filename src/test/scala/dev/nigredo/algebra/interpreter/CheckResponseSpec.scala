package dev.nigredo.algebra.interpreter

import dev.nigredo.algebra.Instruction.Response
import dev.nigredo.compiler.IR._
import org.specs2.mutable.Specification

class CheckResponseSpec extends Specification {

  "Check response" should {
    "pass eq successfully" in {
      val strCase = Response("", 201, Map("field1" -> "str1"))
      val boolCase = Response("", 201, Map("field1" -> "true"))
      val numCase = Response("", 201, Map("field1" -> "1"))

      val strCheck = Check(ResponseHeader, FieldAssertion("field1", Eq, StringLiteral("str1")))
      val boolCheck = Check(ResponseHeader, FieldAssertion("field1", Eq, BooleanLiteral(true)))
      val numCheck = Check(ResponseHeader, FieldAssertion("field1", Eq, NumberLiteral(1)))

      val strActual = checkResponse(Seq(strCheck))(strCase)
      val boolActual = checkResponse(Seq(boolCheck))(boolCase)
      val numActual = checkResponse(Seq(numCheck))(numCase)

      strActual.isValid must beTrue
      strActual.exists(_ == strCase) must beTrue

      boolActual.isValid must beTrue
      boolActual.exists(_ == boolCase) must beTrue

      numActual.isValid must beTrue
      numActual.exists(_ == numCase) must beTrue
    }
    "not pass if response header was not found" in {
      val checks = Seq(Check(ResponseHeader, FieldAssertion("field2", Eq, NumberLiteral(1))))
      val response = Response("", 201, Map("field" -> "1"))

      checkResponse(checks)(response).isInvalid must beTrue
    }
    "not pass if response header value cant'be cast" in {
      val checks = Seq(Check(ResponseHeader, FieldAssertion("field2", Eq, NumberLiteral(1))))
      val response = Response("", 201, Map("field" -> "abc"))

      checkResponse(checks)(response).isInvalid must beTrue
    }
  }
}