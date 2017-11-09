package dev.nigredo.algebra

import cats.InjectK
import cats.data.EitherK
import cats.free.Free
import cats.free.Free._
import dev.nigredo.Model.Response

sealed trait ResponseBody[A]

final case class ExtractResponseBody(response: Response) extends ResponseBody[String]

object ResponseBody {
  type Program[A] = EitherK[Assertion, ResponseBody, A]
}

class ResponseBodyOps[F[_]](implicit I: InjectK[ResponseBody, F]) {
  def extractResponseBody(response: Response): Free[F, String] = inject[ResponseBody, F](ExtractResponseBody(response))
}

object ResponseBodyOps {
  implicit def responseBodyOp[F[_]](implicit I: InjectK[ResponseBody, F]): ResponseBodyOps[F] = new ResponseBodyOps[F]
}
