package dev.nigredo.algebra

import cats.free.Free
import cats.free.Free._
import dev.nigredo.ResponseModel

sealed trait Request[A] {
  val url: String
  val headers: Headers
}

final case class Get(url: String, headers: Headers = Map.empty) extends Request[ResponseModel]

final case class Post(url: String, body: String, headers: Headers = Map.empty) extends Request[ResponseModel]

final case class Put(url: String, body: String, headers: Headers = Map.empty) extends Request[ResponseModel]

final case class Delete(url: String, headers: Headers = Map.empty) extends Request[ResponseModel]

object Request {

  type Program[A] = Free[Request, A]

  def get(url: String)(headers: Headers): Program[ResponseModel] =
    liftF[Request, ResponseModel](Get(url, headers))

  def delete(url: String)(headers: Headers): Program[ResponseModel] =
    liftF[Request, ResponseModel](Delete(url, headers))

  def post(url: String)(headers: Headers)(body: String): Program[ResponseModel] =
    liftF[Request, ResponseModel](Post(url, body, headers))

  def put(url: String)(headers: Headers)(body: String): Program[ResponseModel] =
    liftF[Request, ResponseModel](Put(url, body, headers))
}
