package dev.nigredo

import cats.effect._
import dev.nigredo.Model.Snippet
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.server.blaze.BlazeBuilder

object HttpApp {

  def main(args: Array[String]): Unit = {

    implicit val decoder = jsonOf[IO, Snippet]

    val service = HttpService[IO] {
      case req@POST -> Root / "snippet" =>
        for {
          snippet <- req.as[Snippet]
          resp <- Ok(snippet.asJson)
        } yield resp
    }

    val builder = BlazeBuilder[IO].bindHttp(8080).mountService(service, "/").start
    val r = builder.unsafeRunSync()
  }
}
