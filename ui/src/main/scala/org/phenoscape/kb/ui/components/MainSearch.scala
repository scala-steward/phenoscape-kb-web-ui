package org.phenoscape.kb.ui.components

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.{EntityPage, GeneTaxonSimilarityPage, Page, TaxonPage}
import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Util.StringOps
import org.phenoscape.kb.ui.{KBAPI, Vocab}

object MainSearch {

  def mainSearch(updates: WriteBus[Page], state: String = "default"): Div = {
    val searchTextBus = new EventBus[Option[String]]()
    val editingTextBus = new EventBus[String]()
    val textChanges: EventStream[Option[String]] = editingTextBus.events.mapTo(None)
    val searchTextOpt: Signal[Option[String]] = EventStream.merge(textChanges, searchTextBus.events).startWith(None)
    val hidePanel = searchTextOpt.map(_.isEmpty)
    val searches = searchTextOpt.map {
      case Some(text) => (
        KBAPI.ontologyClassSearch(text, Some(IRI(Vocab.Uberon)), 20),
        KBAPI.ontologyClassSearch(text, Some(IRI(Vocab.VTO)), 20),
        KBAPI.geneSearch(text, 20))
      case None       => (EventStream.empty, EventStream.empty, EventStream.empty)
    }
    val anatomyLinks = for {
      results <- searches
      (termsObs, _, _) = results
      terms <- termsObs.startWith(Nil)
    } yield terms.map { term =>
      li(a(
        role := "button",
        onClick.mapTo(EntityPage(term.iri)) --> updates,
        term.label)
      )
    }
    val taxonLinks = for {
      results <- searches
      (_, termsObs, _) = results
      terms <- termsObs.startWith(Nil)
    } yield terms.map { term =>
      li(a(
        role := "button",
        onClick.mapTo(TaxonPage(term.iri)) --> updates,
        term.label))
    }
    val geneLinks = for {
      result <- searches
      (_, _, genesObs) = result
      genes <- genesObs.startWith(Nil)
    } yield genes.map { gene =>
      li(a(
        onClick.mapTo(GeneTaxonSimilarityPage(geneIRIOpt = Some(gene.iri))) --> updates,
        gene.label, " ", small("(", gene.taxon.label, ")")))
    }

    div(
      cls := s"panel panel-$state",
      div(
        cls := "panel-body",
        div(
          cls := "form-group",
          label(forId := "mainSearch", "Find an item by name"),
          div(
            cls := "input-group",
            input(
              idAttr := "mainSearch",
              tpe := "search",
              cls := "form-control input-sm",
              placeholder := "Taxon, anatomical structure, or gene",
              onChange.mapToValue.map(_.stripToOption) --> searchTextBus,
              controlled(
                value <-- editingTextBus,
                onInput.mapToValue --> editingTextBus
              )
            ),
            span(
              cls := "input-group-btn",
              button(
                onClick.mapTo("") --> editingTextBus,
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
            searchPicker("Genes", geneLinks)))))
  }

  private def searchPicker(title: String, links: Observable[List[HtmlElement]]): HtmlElement = {
    div(
      cls := "search-picker-group panel panel-default",
      div(
        cls := "panel-heading",
        h4(
          cls := "panel-title",
          span(cls.toggle("text-muted") <-- links.map(_.isEmpty), title))),
      div(
        cls := "panel-body",
        ul(
          cls := "list-inline",
          children <-- links)))
  }

}
