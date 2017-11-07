package dev.nigredo

import cats.effect.IO
import dev.nigredo.Model.Snippet
import dev.nigredo.algebra.Instruction
import dev.nigredo.algebra.interpreter.DefaultInterpreter
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.client.blaze._
import org.http4s.dsl.io._
import org.http4s.{Method, Request}

object App {

  implicit val decoder = jsonOf[IO, Snippet]

  def main(args: Array[String]): Unit = {

    val httpClient = PooledHttp1Client[IO]()
    val req = Request[IO](Method.POST, uri("http://localhost:8080/snippet")).withBody(Snippet("hello").asJson)
    println(httpClient.expect(req)(jsonOf[IO, Snippet]).unsafeRunSync)
    httpClient.shutdownNow()
    import dev.nigredo.compiler.Parser._
    import fastparse.all._

    program.parse("Send get request to http://localhost:8080/hello?name=andrii&surname=ivanov") match {
      case Parsed.Failure(expected, _, extra) => println(s"Error: $expected, $extra")
      case Parsed.Success(value, _) =>
        println(value.map(x => Instruction(x).foldMap(DefaultInterpreter)))
    }
  }
}
