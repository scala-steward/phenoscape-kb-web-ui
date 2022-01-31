package org.phenoscape.kb.ui

import com.raquo.airstream.core.EventStream
import io.circe.Decoder
import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
import sttp.client3.circe.asJson
import sttp.client3.{FetchBackend, basicRequest}
import sttp.model.Uri

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
  * Limit the number of concurrent requests to the API, and queue up others.
  * Without throttling we can run into this issue causing dropped requests:
  * https://trac.nginx.org/nginx/ticket/2155
  * This implementation is not at all thread-safe; assuming Javascript environment.
  */
class Throttler(val concurrent: Int) {

  private val backend = FetchBackend()

  private case class Request[T](uri: Uri, promise: Promise[T])(retrieve: => Future[T]) {

    def submit(): Future[T] = {
      val fut = retrieve
      fut.onComplete(_ => checkForDequeue())
      promise.completeWith(fut)
      fut
    }

  }

  private var queue: scala.collection.immutable.Queue[Request[_]] = Queue.empty
  private var inFlight: Set[Future[_]] = Set.empty

  private def checkForDequeue(): Unit = {
    inFlight = inFlight.filterNot(_.isCompleted)
    val numberToDequeue = Math.max(concurrent - inFlight.size, 0)
    val toSubmit = queue.take(numberToDequeue).toList
    queue = queue.drop(numberToDequeue)
    toSubmit.foreach { request =>
      inFlight += request.submit()
    }
  }

  def get[T](uri: Uri)(implicit evidence: Decoder[T]): EventStream[T] = {
    val promise = Promise[T]()
    queue = queue.enqueue(Request(uri, promise)(getFuture[T](uri)))
    checkForDequeue()
    EventStream.fromFuture(promise.future, true)
  }

  private def getFuture[T](uri: Uri)(implicit evidence: Decoder[T]): Future[T] = {
    val future = basicRequest
      .header("Accept", "application/json")
      .get(uri).response(asJson[T])
      .readTimeout(5.minutes)
      .send(backend).map { response =>
      response.body match {
        case Right(_)    => ()
        case Left(error) => println(error.getMessage)
          println(s"Failed decoding JSON response: ${response.body.toString.take(100)}...")
      }
      response.body
    }
      .collect {
        case Right(value) => value
      }
    future
  }

}
