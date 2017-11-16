package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Model.SimilarityMatch
import org.phenoscape.kb.ui.Vocab._

import outwatch.dom._
import outwatch.dom.Attributes.style
import outwatch.dom.VNode
import outwatch.redux.Component
import outwatch.redux.Store
import rxscalajs.Observable

object GeneTaxonSimilarityPage extends Component {

  sealed trait Action
  case class Init(geneIRI: IRI) extends Action
  case class SelectMatch(matched: SimilarityMatch) extends Action

  case class State(geneIRI: IRI, selectedMatch: Option[SimilarityMatch]) extends ComponentState {

    def evolve = {
      case Init(iri)            => copy(geneIRI = iri)
      case SelectMatch(matched) => copy(selectedMatch = Some(matched))
    }

  }

  def apply(initState: State): VNode = {
    view(Store.create(Seq.empty, initState))
  }

  def view(store: Store[State, Action]): VNode = {
    val obsGeneIRI = store.map(_.geneIRI).distinctUntilChanged
    val obsSubject = obsGeneIRI.flatMap(KBAPI.gene)
    val similarityMatches = obsGeneIRI.flatMap { iri =>
      KBAPI.similarityMatches(iri, IRI(Vocab.TaxonSimilarityCorpus), 20, 0).map(_.results) //FIXME offset will depend on page
    }
    val selectedMatch = store.map(_.selectedMatch).distinctUntilChanged
    val hasMatchSelection = selectedMatch.map(_.nonEmpty)
    val queryProfileSize = obsGeneIRI.flatMap(KBAPI.similarityProfileSize)
    val selectedMatchProfileSizeOpt = selectedMatch.flatMap(matchedOpt => Util.sequence(matchedOpt.map(matched => KBAPI.similarityProfileSize(matched.matchProfile.iri))))
    val selectedMatchAsTaxon = selectedMatch.flatMap(matchedOpt => Util.sequence(matchedOpt.map(matched => KBAPI.taxon(matched.matchProfile.iri))))
    div(
      h2("Similar evolutionary variation"),
      p("These taxonomic groups vary in phenotypes that match most closely to the gene profile (collection of phenotypes) that result when the action of this gene is disrupted (e.g., knocked down)."),
      div(
        cls := "panel-body",
        div(
          cls := "row",
          h4("Evolutionary profiles matching gene ", b(span(child <-- obsSubject.map(_.label))), " (", span(child <-- obsSubject.map(_.taxon.label)), ")"),
          div(
            cls := "col-sm-4",
            div(hidden <-- similarityMatches.map(_.nonEmpty), i("No matches")),
            div(
              hidden <-- similarityMatches.map(_.isEmpty),
              table(
                cls := "table table-condensed",
                thead(
                  tr(
                    th("Group"),
                    th("Taxon"),
                    th("Expect Score\u00A0", span(cls := "glyphicon glyphicon-info-sign")), //FIXME popover
                    th())),
                tbody(children <-- similarityMatches.map(_.map(matchRow(_, store))))))),
          div(
            cls := "col-sm-8",
            div(
              cls := "well",
              hidden <-- hasMatchSelection,
              h2(cls := "text-center", i(cls := "small", "Click on any row to the left to view detailed results"))),
            div(
              cls := "panel panel-default",
              style := "margin-top: 1em;",
              hidden <-- hasMatchSelection.map(!_),
              div(
                cls := "panel-heading",
                h4(cls := "panel-title", "Match details")),
              div(
                cls := "panel-body",
                form(
                  cls := "form-horizontal kb-form-condensed",
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Searched gene:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child <-- obsSubject.map(_.label)), " (", span(child <-- obsSubject.map(_.taxon.label)), ") ",
                        small(a(
                          href := "#", //FIXME link to phenotype profile
                          span(child <-- queryProfileSize),
                          " phenotypes"))))),
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Matched taxon:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child <-- selectedMatchAsTaxon.map(_.map(Views.taxonName).getOrElse(span()))), " ",
                        small(a(
                          href := "#", //FIXME link to phenotype profile
                          span(child <-- selectedMatchProfileSizeOpt.map(_.map(_.toString).getOrElse(""))),
                          " phenotypes"))))),
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Searched gene:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child <-- obsSubject.map(_.label)), " (", span(child <-- obsSubject.map(_.taxon.label)), ") ",
                        small(a(
                          href := "#", //FIXME link to phenotype profile
                          span(child <-- queryProfileSize),
                          " phenotypes"))))),
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Searched gene:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child <-- obsSubject.map(_.label)), " (", span(child <-- obsSubject.map(_.taxon.label)), ") ",
                        small(a(
                          href := "#", //FIXME link to phenotype profile
                          span(child <-- queryProfileSize),
                          " phenotypes"))))))))))))
  }

  private def matchRow(matched: SimilarityMatch, store: Store[State, Action]): VNode = {
    val taxonName = KBAPI.taxon(matched.matchProfile.iri).map(Views.taxonName)
    val hover = createBoolHandler(false)
    val selected = store.map(_.selectedMatch == Some(matched))
    val hoverOrSelected = hover.combineLatestWith(selected)((hov, sel) => hov || sel)
    val classes = Util.observableCSS(hover.map("active" -> _).merge(selected.map("info" -> _)))
    //val group = ???
    tr(
      cls <-- classes,
      mouseenter(true) --> hover,
      mouseleave(false) --> hover,
      click(SelectMatch(matched)) --> store,
      td("TODO group"),
      td(child <-- taxonName),
      td(cls := "text-center", matched.expectScore.toString), //FIXME format score
      td(cls := "match-list-arrow", span(hidden <-- hoverOrSelected.map(!_), "➠")))
  }

}