package dev.nigredo.compiler.model

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