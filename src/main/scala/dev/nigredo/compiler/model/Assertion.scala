package dev.nigredo.compiler.model

import dev.nigredo.compiler.Literal

sealed trait Assertion

final case class And(lft: Assertion, rgt: Assertion) extends Assertion

final case class Or(lft: Assertion, rgt: Assertion) extends Assertion

final case class Not(expr: Assertion) extends Assertion

final case class FieldAssertion(name: String, op: Operation, value: Literal) extends Assertion

object FieldAssertion {
  def apply(data: (String, Operation, Literal)): Assertion = new FieldAssertion(data._1, data._2, data._3)
}