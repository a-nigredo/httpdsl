package dev.nigredo.algebra

import cats.data.Validated
import cats.free.Free
import cats.free.Free.liftF
import dev.nigredo.compiler.IR.{Assertion, IR, Request}
import dev.nigredo.algebra.Instruction.{Response, Validation}

sealed trait Instruction[A]

case class SendRequest(request: Request) extends Instruction[Response]

case class CheckResponse(response: Response, assertion: Seq[Assertion]) extends Instruction[Validation]

case class Result(data: Validation) extends Instruction[String]

object Instruction {

  case class Response(body: String, code: Int, headers: Map[String, String] = Map.empty)

  type FreeResult[A] = Free[Instruction, A]
  type Validation = Validated[Vector[String], Response]

  def sendRequest(request: Request): FreeResult[Response] =
    liftF[Instruction, Response](SendRequest(request))

  def checkResponse(assertion: Seq[Assertion], response: Response): FreeResult[Validation] =
    liftF[Instruction, Validation](CheckResponse(response, assertion))

  def result(validation: Validation): FreeResult[String] =
    liftF[Instruction, String](Result(validation))

  def apply(repr: IR): Free[Instruction, String] = {
    val (request, assertion) = repr
    val requestOp = sendRequest(request)
    if (assertion.isEmpty)
      for {
        response <- requestOp
        res <- result(Validated.valid(response))
      } yield res
    else
      for {
        response <- requestOp
        check <- checkResponse(assertion, response)
        result <- result(check)
      } yield result
  }
}
