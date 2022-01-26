package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._

trait RouteComponent {

  type Actions

  def actions: Actions

  def view: HtmlElement

}
