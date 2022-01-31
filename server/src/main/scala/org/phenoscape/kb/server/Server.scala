package org.phenoscape.kb.server

import org.http4s.HttpRoutes._
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import zio._
import zio.interop.catz._
import zio.interop.catz.implicits._

object Server extends App {

  val KBEndpoint: String = sys.env.getOrElse("KB_ENDPOINT", "https://kb.phenoscape.org/api/v2-beta")

  private val dsl = Http4sDsl[Task]

  import dsl._

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = server.exitCode

  val server: RIO[ZEnv, Unit] =
    ZIO.runtime[Any].flatMap { implicit runtime =>
      BlazeServerBuilder[Task]
        .withExecutionContext(runtime.platform.executor.asEC)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(Router("/" -> routes).orNotFound)
        .serve
        .compile
        .drain
    }

  val routes: HttpRoutes[Task] = of[Task] {
    case request @ GET -> Root / "js" / path if path == "conf.js"                                                    =>
      Ok(s"var KB_ENDPOINT = '$KBEndpoint';")
    case request @ GET -> Root / "js" / path if List(".js", ".css", ".map", ".html", ".webm").exists(path.endsWith)  =>
      StaticFile.fromResource[Task]("/js/" + path, Some(request))
        .getOrElseF(NotFound())
    case request @ HEAD -> Root / "js" / path if List(".js", ".css", ".map", ".html", ".webm").exists(path.endsWith) =>
      StaticFile.fromResource[Task]("/js/" + path, Some(request))
        .getOrElseF(NotFound())
    case request @ GET -> Root / "css" / path if List(".css").exists(path.endsWith)                                  =>
      StaticFile.fromResource[Task]("/css/" + path, Some(request))
        .getOrElseF(NotFound())
    case request @ HEAD -> Root / "css" / path if List(".css").exists(path.endsWith)                                 =>
      StaticFile.fromResource[Task]("/css/" + path, Some(request))
        .getOrElseF(NotFound())
    case request @ GET -> Root / "img" / path if List(".gif", ".png").exists(path.endsWith)                          =>
      StaticFile.fromResource[Task]("/img/" + path, Some(request))
        .getOrElseF(NotFound())
    case request @ HEAD -> Root / "img" / path if List(".gif", ".png").exists(path.endsWith)                         =>
      StaticFile.fromResource[Task]("/img/" + path, Some(request))
        .getOrElseF(NotFound())
    case request @ GET -> path                                                                                       =>
      println(path)
      StaticFile.fromResource[Task]("/index.html", Some(request))
        .getOrElseF(NotFound())
  }

}
