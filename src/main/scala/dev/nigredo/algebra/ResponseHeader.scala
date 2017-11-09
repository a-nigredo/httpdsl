package dev.nigredo.algebra

import cats.InjectK
import cats.data.EitherK
import cats.free.Free
import cats.free.Free.inject
import dev.nigredo.Model.Response

sealed trait ResponseHeader[A]

final case class ExtractResponseHeaders(response: Response) extends ResponseHeader[Map[String, String]]

final case class ExtractResponseHeaderValue(headers: Map[String, String], fieldName: String) extends ResponseHeader[String]

object ResponseHeader {
  type Program[A] = EitherK[Assertion, ResponseHeader, A]
}

class ResponseHeaderOps[F[_]](implicit I: InjectK[ResponseHeader, F]) {

  def extractResponseHeaders(response: Response): Free[F, Map[String, String]] =
    inject[ResponseHeader, F](ExtractResponseHeaders(response))

  def extractResponseHeaderValue(headers: Map[String, String])(fieldName: String): Free[F, String] =
    inject[ResponseHeader, F](ExtractResponseHeaderValue(headers, fieldName))
}

object ResponseHeaderOps {
  implicit def responseHeaderOp[F[_]](implicit I: InjectK[ResponseHeader, F]): ResponseHeaderOps[F] = new ResponseHeaderOps[F]
}