package dev.nigredo.compiler.model

sealed trait Check {
  val assertion: Assertion
}

final case class CheckResponseBody(assertion: Assertion) extends Check

final case class CheckResponseHeader(assertion: Assertion) extends Check
