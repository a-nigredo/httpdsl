package dev.nigredo

object Model {

  final case class Snippet(value: String)

  final case class Response(body: String, code: Int, headers: Map[String, String] = Map.empty)

  final case class Error(value: String) extends AnyVal
}
