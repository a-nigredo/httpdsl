package dev.nigredo.algebra

import cats.InjectK
import cats.free.Free
import cats.free.Free._
import dev.nigredo.compiler.IR.Operation

final case class CompareValue[A](operation: Operation, value1: String, value2: A)

sealed trait Assertion[A]

final case class AssertionAsInt(compare: CompareValue[Int]) extends Assertion[Boolean]

final case class AssertionAsBoolean(compare: CompareValue[Boolean]) extends Assertion[Boolean]

final case class AssertionAsString(compare: CompareValue[String]) extends Assertion[Boolean]

final case class AssertionAnd[A](lc: Assertion[A], rc: Assertion[A]) extends Assertion[Boolean]

final case class AssertionOr[A](lc: Assertion[A], rc: Assertion[A]) extends Assertion[Boolean]

final case class AssertionNot[A](compare: Assertion[A]) extends Assertion[Boolean]

final case class AssertionResult(boolean: Boolean) extends Assertion[String]

class AssertionOps[F[_]](implicit I: InjectK[Assertion, F]) {

  def asInt(value: CompareValue[Int]): Free[F, Boolean] = inject[Assertion, F](AssertionAsInt(value))

  def asString(value: CompareValue[String]): Free[F, Boolean] = inject[Assertion, F](AssertionAsString(value))

  def asBoolean(value: CompareValue[Boolean]): Free[F, Boolean] = inject[Assertion, F](AssertionAsBoolean(value))

  def and[A](lc: Assertion[A])(rc: Assertion[A]): Free[F, Boolean] = inject[Assertion, F](AssertionAnd[A](lc, rc))

  def or[A](lc: Assertion[A])(rc: Assertion[A]): Free[F, Boolean] = inject[Assertion, F](AssertionOr[A](lc, rc))

  def not[A](lc: Assertion[A]): Free[F, Boolean] = inject[Assertion, F](AssertionNot(lc))

  def result(boolean: Boolean): Free[F, String] = inject[Assertion, F](AssertionResult(boolean))
}

object AssertionOps {
  implicit def assertionOps[F[_]](implicit I: InjectK[Assertion, F]): AssertionOps[F] = new AssertionOps[F]
}
