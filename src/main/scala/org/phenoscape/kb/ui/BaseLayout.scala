package org.phenoscape.kb.ui

import outwatch.dom._
import rxscalajs.Observable

object BaseLayout {

  def view(node: Observable[VNode]): VNode = {
    div(
      cls := "container-fluid",
      div(
        cls := "row bottom-buffer",
        div(
          cls := "col-xs-8",
          div(
            cls := "btn-toolbar",
            h1(a(href := "#/home", alt := "Phenoscape Knowledgebase", img(id := "phenoscape_logo", src := "img/phenoscape_logo.png"))),
            div(
              cls := "btn-group",
              button(tpe := "button", cls := "btn btn-default btn-sm dropdown-toggle", data.toggle := "dropdown", Aria.expanded := false,
                "About ", span(cls := "caret")),
              ul(cls := "dropdown-menu", role := "menu",
                li(a(href := "#/about/phenoscape", "Phenoscape")),
                li(a(href := "#/about/phenoscape/kb", "Phenoscape Knowledgebase")))),
            a(
              cls := "btn btn-primary btn-sm",
              role := "button",
              href := "https://docs.google.com/forms/d/1tBoctX6qMLsm58raEW3Z2bNV1i6gVYaPCdZ2Ab28EbM/edit",
              target := "_blank",
              i("Feedback"))))),

      div(child <-- node),

      div(
        cls := "row",
        div(
          cls := "col-xs-12",
          div(
            cls := "panel panel-default",
            div(
              cls := "panel-body",
              h6("The Phenoscape project is funded by NSF grants DBI-1062404 and DBI-1062542, and supported by the National Evolutionary Synthesis Center (NESCent), NSF EF-0905606."))))))
  }

}
