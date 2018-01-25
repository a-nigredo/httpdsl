package dev.nigredo.algebra

import cats.InjectK
import cats.data.EitherK
import cats.free.Free
import cats.free.Free._
import dev.nigredo.ResponseModel

sealed trait Response[A]

final case class ExtractBody(response: ResponseModel) extends Response[String]

final case class ExtractHeader(response: ResponseModel, key: String) extends Response[String]

final case class ExtractStatusCode(response: ResponseModel) extends Response[Int]

object Response {
  type Program[A] = EitherK[Assertion, Response, A]
}

class ResponseOps[F[_]](implicit I: InjectK[Response, F]) {

  def extractBody(response: ResponseModel): Free[F, String] = inject(ExtractBody(response))

  def extractHeader(response: ResponseModel)(key: String): Free[F, String] = inject(ExtractHeader(response, key))

  def extractStatusCode(response: ResponseModel): Free[F, Int] = inject(ExtractStatusCode(response))
}

object ResponseOps {
  implicit def responseBody[F[_]](implicit I: InjectK[Response, F]): ResponseOps[F] = new ResponseOps[F]
}