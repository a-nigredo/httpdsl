package dev.nigredo.compiler

object IR {

  type IR = (Request, Seq[Check])

  sealed trait Literal

  final case class StringLiteral(value: String) extends Literal

  final case class BooleanLiteral(value: Boolean) extends Literal

  final case class NumberLiteral(value: Int) extends Literal

  final case class ArrayLiteral(value: Seq[Literal]) extends Literal

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

  sealed trait AssertionTarget

  case object ResponseBody extends AssertionTarget

  case object ResponseHeader extends AssertionTarget

  object AssertionTarget {
    def apply(str: String): AssertionTarget =
      str match {
        case "body" => ResponseBody
        case "header" => ResponseHeader
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

  final case class Check(target: AssertionTarget, assertion: Assertion)

  object Check {
    def apply(data: (AssertionTarget, Assertion)): Check = new Check(data._1, data._2)
  }

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
