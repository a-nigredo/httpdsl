package dev.nigredo.algebra.interpreter

import cats.data.Validated
import cats.{Id, ~>}
import dev.nigredo.algebra.{CheckResponse, Instruction, Result, SendRequest}

object DefaultInterpreter extends (Instruction ~> Id) {
  override def apply[A](fa: Instruction[A]): Id[A] = {
    fa match {
      case SendRequest(request) =>
        Instruction.Response("str", 200, Map.empty)
      case CheckResponse(assertions, response) =>
        Validated.invalid(Vector("asdsada"))
      case Result(data) => data.fold(_.mkString(","), _.body)
    }
  }
}
