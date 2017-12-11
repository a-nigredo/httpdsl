package dev.nigredo.compiler.model

final case class Request(url: String, method: Method, headers: Map[String, String])