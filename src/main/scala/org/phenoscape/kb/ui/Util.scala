package org.phenoscape.kb.ui

import outwatch.Sink
import outwatch.dom.Handler
import rxscalajs.Observable
import Model.IRI

object Util {

  /**
   * A handler for toggling CSS classes in response to events
   */
  def createCSSClassHandler(initialClasses: String*): Handler[(String, Boolean)] = Sink.createHandler[(String, Boolean)](initialClasses.map(cls => cls -> true): _*)

  /**
   * Map CSS class toggles to a usable attribute value
   */
  def observableCSS(toggles: Observable[(String, Boolean)]): Observable[String] = toggles.scan(Set.empty[String]) { (classes, action) =>
    action match {
      case (cls, true)  => classes + cls
      case (cls, false) => classes - cls
    }
  }.map(_.mkString(" "))

  def sequence[T](optObs: Option[Observable[T]]): Observable[Option[T]] = optObs match {
    case Some(obs) => obs.map(Option(_))
    case None      => Observable.of(None)
  }

  def taxonThumbnailIRI(phylopic: IRI): IRI = {
    val uuid = phylopic.id.replaceAllLiterally("http://phylopic.org/image/", "").replaceAllLiterally("/", "")
    IRI(s"http://phylopic.org/assets/images/submissions/$uuid.64.png")
  }

}