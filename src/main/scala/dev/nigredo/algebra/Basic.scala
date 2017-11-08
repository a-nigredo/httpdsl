package dev.nigredo.algebra

import dev.nigredo.Model.Response
import dev.nigredo.compiler.IR.{Check, Request}

//TODO think about new name
sealed trait Basic[A]

case class SendRequest(request: Request) extends Basic[Response]
