package org.phenoscape.kb.ui

import outwatch.Sink
import outwatch.dom.Handler
import rxscalajs.Observable

object Util {

  /**
   * A handler for toggling CSS classes in response to events
   */
  def createCSSClassHandler(initialClasses: String*): Handler[(String, Boolean)] = Sink.createHandler[(String, Boolean)](initialClasses.map(cls => cls -> true): _*)

  /**
   * Map a CSS class handler to a usable attribute value
   */
  def observableCSS(handler: Handler[(String, Boolean)]): Observable[String] = handler.scan(Set.empty[String]) { (classes, action) =>
    action match {
      case (cls, true)  => classes + cls
      case (cls, false) => classes - cls
    }
  }.map(_.mkString(" "))

}