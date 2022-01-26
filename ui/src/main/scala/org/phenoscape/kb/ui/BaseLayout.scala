package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.{AboutPage, HomePage, Page}

object BaseLayout {

  def view(updates: WriteBus[Page], $childNode: Signal[HtmlElement]): Div = {
    val $kbData = KBAPI.kbInfo
    val $dateString = $kbData.map(_.buildDate.toLocaleDateString())
    div(
      cls := "container-fluid",
      div(
        cls := "row bottom-buffer",
        div(
          cls := "col-xs-8",
          div(
            cls := "btn-toolbar",
            h1(a(
              role := "button",
              onClick.mapTo(HomePage) --> updates,
              alt := "Phenoscape Knowledgebase",
              img(idAttr := "phenoscape_logo", src := "/img/phenoscape_logo.png"))),
            div(
              cls := "btn-group",
              button(tpe := "button", cls := "btn btn-default btn-sm dropdown-toggle", dataAttr("toggle") := "dropdown", aria.expanded := false,
                "About ", span(cls := "caret")),
              ul(cls := "dropdown-menu", role := "menu",
                li(a(href := "http://phenoscape.org", "Phenoscape")),
                li(a(
                  role := "button",
                  onClick.mapTo(AboutPage) --> updates,
                  "Phenoscape Knowledgebase")))),
            a(
              cls := "btn btn-primary btn-sm",
              role := "button",
              href := "https://docs.google.com/forms/d/1tBoctX6qMLsm58raEW3Z2bNV1i6gVYaPCdZ2Ab28EbM/edit",
              target := "_blank",
              i("Feedback")))),
        div(
          cls := "col-xs-4",
          div(
            cls := "panel panel-default kb-stats-panel top-buffer",
            div(cls := "panel-heading",
              h5(styleAttr := "margin-top: 0px; margin-bottom: 0px", child <-- $dateString.map(date => s"Current release: $date"))
            ),
            table(
              cls := "table table-condensed",
              tbody(
                tr(
                  cls := "text-right",
                  td("Annotated matrices:"), td(child <-- $kbData.map(_.annotatedMatrices))),
                tr(
                  cls := "text-right",
                  td("Annotated taxa:"), td(child <-- $kbData.map(_.annotatedTaxa))),
                tr(
                  cls := "text-right",
                  td("Annotated characters:"), td(child <-- $kbData.map(_.annotatedCharacters))),
                tr(
                  cls := "text-right",
                  td("Annotated character states:"), td(child <-- $kbData.map(_.annotatedStates)))))))),
      div(child <-- $childNode),
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
