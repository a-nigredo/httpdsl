package dev.nigredo.model

final case class Response(body: String, code: Int, headers: Map[String, String] = Map.empty)