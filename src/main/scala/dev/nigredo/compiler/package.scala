package dev.nigredo

import dev.nigredo.compiler.model.{Check, Request}
import iota.TList.::
import iota.{Cop, TNil}

package object compiler {

  type AST = (Request, Seq[Check])
  type Literal = Cop[Seq[Value] :: String :: Int :: Boolean :: TNil]

  final case class Value(value: Literal)

  object Value {
    implicit def Value2Literal(value: Value): Literal = value.value

    implicit def SeqValue2SeqLiteral(value: Seq[Value]): Seq[Literal] = value.map(_.value)

    implicit def Literal2Value(literal: Literal): Value = Value(literal)

    implicit def SeqLiteral2SeqValue(literal: Seq[Literal]): Seq[Value] = literal.map(Value.apply)
  }

  object Literal {
    val String: Cop.Inject[String, Literal] = Cop.Inject[String, Literal]
    val Int: Cop.Inject[Int, Literal] = Cop.Inject[Int, Literal]
    val Boolean: Cop.Inject[Boolean, Literal] = Cop.Inject[Boolean, Literal]
    val Array: Cop.Inject[Seq[Value], Literal] = Cop.Inject[Seq[Value], Literal]

    def apply(value: String): Literal = String(value)

    def apply(value: Int): Literal = Int(value)

    def apply(value: Boolean): Literal = Boolean(value)

    def apply(value: Seq[Value]): Literal = Array(value)
  }

}
