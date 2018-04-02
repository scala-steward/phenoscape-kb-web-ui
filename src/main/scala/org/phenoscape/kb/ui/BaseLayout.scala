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
              h6(
                "The Phenoscape project is currently funded by NSF ABI Innovation collaborative grants (",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1661529", "1661529"), ", ",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1661356", "1661356"), ", ",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1661456", "1661456"), ", ",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1661516", "1661516"),
                ") and an ABI Development grant (",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1062542", "1062542"),
                "). Phenoscape was previously funded by NSF ",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1062404", "1062404"), "  and ",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=0641025", "0641025"), ", and supported by NESCent, NSF ",
                a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=0905606", "0905606"), "."))))))
  }

}
