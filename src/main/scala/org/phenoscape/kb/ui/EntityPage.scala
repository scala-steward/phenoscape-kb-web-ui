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

object EntityPage extends Component {

  sealed trait Action
  case class Init(entityIRI: IRI) extends Action

  case class State(entityIRI: IRI) extends ComponentState {

    def evolve = {
      case Init(iri) => copy(entityIRI = iri)
    }

  }

  def apply(initState: State): VNode = {
    view(Store.create(Seq.empty, initState))
  }

  def view(store: Store[State, Action]): VNode = {
    import outwatch.dom._

    val obsTermInfo: Observable[Model.Term] = store.flatMap(s => KBAPI.termLabel(s.entityIRI))
    val obsClassificationData = store.flatMap(s => KBAPI.classification(s.entityIRI, IRI(Uberon)))
    def termLink(term: Term) = a(href := s"#/entity/${Vocab.compact(term.iri).id}", term.label)

    div(
      h2(
        span(cls := "badge", "Taxon"), " ",
        span(child <-- obsTermInfo.map(_.label)),
        small(
          " ",
          a(href <-- store.map(_.entityIRI.id), target := "_blank", cls := "link-no-color",
            child <-- store.map(s => Vocab.compact(s.entityIRI).id)))),
      div(
        cls := "panel panel-default top-buffer",
        div(cls := "panel-body", child <-- obsClassificationData.map(Views.classification(_, termLink)))))
  }

}