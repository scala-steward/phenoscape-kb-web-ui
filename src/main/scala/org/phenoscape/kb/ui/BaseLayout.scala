package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Util.StringOps
import outwatch.dom._
import rxscalajs.Observable

object BaseLayout {

  def view(node: Observable[VNode]): VNode = {
    val searchTextHandler = createHandler[Option[String]](None)
    val editingTextHandler = createStringHandler()
    val textChanges = editingTextHandler.mapTo(None)
    val searchTextOpt = searchTextHandler.merge(textChanges).startWith(None)
    val hidePanel = searchTextOpt.map(_.isEmpty)
    val searches = searchTextOpt.map {
      case Some(text) => (
        KBAPI.ontologyClassSearch(text, Some(IRI(Vocab.Uberon)), 20),
        KBAPI.ontologyClassSearch(text, Some(IRI(Vocab.VTO)), 20),
        KBAPI.geneSearch(text, 20))
      case None       => (Observable.empty, Observable.empty, Observable.empty)
    }
    val anatomyLinks = for {
      (termsObs, _, _) <- searches
      terms <- termsObs.startWith(Nil)
    } yield terms.map { term =>
      li(a(
        click("") --> editingTextHandler,
        href := s"#/entity/${Vocab.compact(term.iri).id}", term.label))
    }
    val taxonLinks = for {
      (_, termsObs, _) <- searches
      terms <- termsObs.startWith(Nil)
    } yield terms.map { term =>
      li(a(
        click("") --> editingTextHandler,
        href := s"#/taxon/${Vocab.compact(term.iri).id}", term.label))
    }
    val geneLinks = for {
      (_, _, genesObs) <- searches
      genes <- genesObs.startWith(Nil)
    } yield genes.map { gene =>
      li(a(
        click("") --> editingTextHandler,
        href := s"#/gene/${Vocab.compact(gene.iri).id}", gene.label, " ", small("(", gene.taxon.label, ")")))
    }

    def searchPicker(title: String, links: Observable[List[VNode]]): VNode = {
      val cssClasses = Util.observableCSS(links.map("text-muted" -> _.isEmpty))
      div(
        cls := "search-picker-group panel panel-default",
        div(
          cls := "panel-heading",
          h4(
            cls := "panel-title",
            span(cls <-- cssClasses, title))),
        div(
          cls := "panel-body",
          ul(
            cls := "list-inline",
            children <-- links)))
    }

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
                li(a(href := "http://phenoscape.org", "Phenoscape")),
                li(a(href := "#/about/phenoscape/kb", "Phenoscape Knowledgebase")))),
            a(
              cls := "btn btn-primary btn-sm",
              role := "button",
              href := "https://docs.google.com/forms/d/1tBoctX6qMLsm58raEW3Z2bNV1i6gVYaPCdZ2Ab28EbM/edit",
              target := "_blank",
              i("Feedback")))),
        div(
          cls := "col-xs-4",
          div(
            cls := "panel panel-default top-buffer",
            div(
              cls := "panel-body",
              div(
                cls := "form-group",
                label(`for` := "mainSearch", "Find an item by name"),
                div(
                  cls := "input-group",
                  input(
                    id := "mainSearch",
                    tpe := "search",
                    cls := "form-control input-sm",
                    placeholder := "Taxon or anatomical structure",
                    change --> searchTextHandler.redirectMap(_.target.value.stripToOption),
                    inputString --> editingTextHandler,
                    value <-- editingTextHandler),
                  span(
                    cls := "input-group-btn",
                    button(
                      click("") --> editingTextHandler,
                      cls := "btn btn-default btn-sm",
                      tpe := "button",
                      "×")))),
              div(
                cls := "search-picker",
                hidden <-- hidePanel,
                div(
                  cls := "panel-group",
                  searchPicker("Anatomical structures", anatomyLinks),
                  searchPicker("Taxa", taxonLinks),
                  searchPicker("Genes", geneLinks))))))),

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
