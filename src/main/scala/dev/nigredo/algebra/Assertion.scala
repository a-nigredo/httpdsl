package dev.nigredo.algebra

import cats.InjectK
import cats.free.Free
import cats.free.Free._
import dev.nigredo.compiler.IR
import dev.nigredo.compiler.IR.Operation

sealed trait Assertion[A]

final case class Expr(operation: Operation, value1: String, value2: IR.Literal)

final case class IsTrue(value: Expr) extends Assertion[Boolean]

final case class And[A](lc: Assertion[A], rc: Assertion[A]) extends Assertion[Boolean]

final case class Or[A](lc: Assertion[A], rc: Assertion[A]) extends Assertion[Boolean]

final case class Not[A](compare: Assertion[A]) extends Assertion[Boolean]

final case class Result(boolean: Boolean) extends Assertion[String]

class AssertionOps[F[_]](implicit I: InjectK[Assertion, F]) {

  def assert(value: Expr): Free[F, Boolean] = inject[Assertion, F](IsTrue(value))

  def and[A](lc: Assertion[A])(rc: Assertion[A]): Free[F, Boolean] = inject[Assertion, F](And[A](lc, rc))

  def or[A](lc: Assertion[A])(rc: Assertion[A]): Free[F, Boolean] = inject[Assertion, F](Or[A](lc, rc))

  def not[A](lc: Assertion[A]): Free[F, Boolean] = inject[Assertion, F](Not(lc))

  def result(boolean: Boolean): Free[F, String] = inject[Assertion, F](Result(boolean))
}

object AssertionOps {
  implicit def assertionOps[F[_]](implicit I: InjectK[Assertion, F]): AssertionOps[F] = new AssertionOps[F]
}
