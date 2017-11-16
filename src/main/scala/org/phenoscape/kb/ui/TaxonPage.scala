package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Model.Term
import org.phenoscape.kb.ui.Model.Taxon
import org.phenoscape.kb.ui.Views.taxonName
import org.phenoscape.kb.ui.Vocab._

import outwatch.dom.VNode
import outwatch.redux.Component
import outwatch.redux.Store
import rxscalajs.Observable

object TaxonPage extends Component {

  sealed trait Action
  case class Init(taxonIRI: IRI) extends Action
  case object ChangeTaxon extends Action

  case class State(taxonIRI: IRI) extends ComponentState {

    def evolve = {
      case Init(iri)   => copy(taxonIRI = iri)
      case ChangeTaxon => ???
    }

  }

  def apply(initState: State): VNode = {
    view(Store.create(Seq.empty, initState))
  }

  def view(store: Store[State, Action]): VNode = {
    import outwatch.dom._

    val obsTaxon = store.flatMap(s => KBAPI.taxon(s.taxonIRI))
    val obsClassificationData = store.flatMap(s => KBAPI.classification(s.taxonIRI, IRI(VTO)))
    def taxonTermToView(term: Term) = a(
      href := s"#/taxon/${Vocab.compact(term.iri).id}",
      child <-- KBAPI.taxon(term.iri).map(Views.taxonName))

    div(
      h2(
        span(cls := "badge", "Taxon"), " ",
        span(child <-- obsTaxon.map(taxonName)),
        span(hidden := true, hidden <-- obsTaxon.map(_.commonName.isEmpty), " (", span(child <-- obsTaxon.map(_.commonName.getOrElse(""))), ")"),
        small(
          " ",
          a(href <-- store.map(_.taxonIRI.id), target := "_blank", cls := "link-no-color",
            child <-- store.map(s => Vocab.compact(s.taxonIRI).id)))),
      div(
        cls := "panel panel-default top-buffer",
        div(cls := "panel-body", child <-- obsClassificationData.map(Views.classification(_, taxonTermToView)))))
  }

}