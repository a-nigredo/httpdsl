package dev.nigredo.compiler.model

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
