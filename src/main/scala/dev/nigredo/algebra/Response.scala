package dev.nigredo.algebra

import cats.InjectK
import cats.data.EitherK
import cats.free.Free
import cats.free.Free._
import dev.nigredo.Model.{Response => ModelResponse}

sealed trait ResponseBody[A]

sealed trait ResponseHeader[A]

final case class ExtractResponseBody(response: ModelResponse) extends ResponseBody[String]

final case class ExtractResponseHeaders(response: ModelResponse) extends ResponseHeader[Map[String, String]]

final case class ExtractResponseHeaderValue(headers: Map[String, String], fieldName: String) extends ResponseHeader[String]

class ResponseHeaderOps[F[_]](implicit I: InjectK[ResponseHeader, F]) {
  def extractResponseHeaders(response: ModelResponse): Free[F, Map[String, String]] = inject[ResponseHeader, F](ExtractResponseHeaders(response))

  def extractResponseHeaderValue(headers: Map[String, String])(fieldName: String): Free[F, String] = inject[ResponseHeader, F](ExtractResponseHeaderValue(headers, fieldName))
}

object ResponseHeaderOps {

  type CheckResponseHeader[A] = EitherK[Assertion, ResponseHeader, A]

  implicit def responseHeaderOp[F[_]](implicit I: InjectK[ResponseHeader, F]): ResponseHeaderOps[F] = new ResponseHeaderOps[F]
}

class ResponseBodyOps[F[_]](implicit I: InjectK[ResponseBody, F]) {
  def extractResponseBody(response: ModelResponse): Free[F, String] = inject[ResponseBody, F](ExtractResponseBody(response))

}

object ResponseBodyOps {

  type CheckResponseBody[A] = EitherK[Assertion, ResponseBody, A]

  implicit def responseBodyOp[F[_]](implicit I: InjectK[ResponseBody, F]): ResponseBodyOps[F] = new ResponseBodyOps[F]
}
