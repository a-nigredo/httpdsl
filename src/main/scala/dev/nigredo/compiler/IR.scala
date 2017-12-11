package dev.nigredo.compiler

import iota.TList.::
import iota._

object IR {

  type IR = (Request, Seq[Check])
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
  }

  sealed trait Operation

  case object Eq extends Operation {
    override def toString = "eq"
  }

  case object Neq extends Operation {
    override def toString: String = "neq"
  }

  case object Gt extends Operation {
    override def toString: String = "gt"
  }

  case object Gte extends Operation {
    override def toString: String = "gte"
  }

  case object Lt extends Operation {
    override def toString: String = "lt"
  }

  case object Lte extends Operation {
    override def toString: String = "lte"
  }

  case object Sw extends Operation {
    override def toString: String = "sw"
  }

  case object Ew extends Operation {
    override def toString: String = "ew"
  }

  case object Consist extends Operation {
    override def toString: String = "consist"
  }

  object Operation {

    val ops = Vector(Eq.toString, Neq.toString, Gt.toString, Gte.toString, Lt.toString, Lte.toString, Consist.toString,
      Sw.toString, Ew.toString)

    def apply(str: String): Operation = str match {
      case "eq" => Eq
      case "neq" => Neq
      case "gt" => Gt
      case "gte" => Gte
      case "lt" => Lt
      case "lte" => Lte
      case "consist" => Consist
      case "sw" => Sw
      case "ew" => Ew
    }
  }

  sealed trait Assertion

  final case class And(lft: Assertion, rgt: Assertion) extends Assertion

  final case class Or(lft: Assertion, rgt: Assertion) extends Assertion

  final case class Not(expr: Assertion) extends Assertion

  final case class FieldAssertion(name: String, op: Operation, value: Literal) extends Assertion

  object FieldAssertion {
    def apply(data: (String, Operation, Literal)): Assertion = new FieldAssertion(data._1, data._2, data._3)
  }

  sealed trait Check {
    val assertion: Assertion
  }

  final case class CheckResponseBody(assertion: Assertion) extends Check

  final case class CheckResponseHeader(assertion: Assertion) extends Check

  sealed trait Method

  case object Get extends Method

  case object Post extends Method

  case object Put extends Method

  case object Delete extends Method

  object Method {
    def apply(str: String): Method = str match {
      case "get" => Get
      case "post" => Post
      case "put" => Put
      case "delete" => Delete
    }
  }

  final case class Request(url: String, method: Method, headers: Map[String, String])

}
