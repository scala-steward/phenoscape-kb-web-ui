package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.IRI
import outwatch.dom.VNode
import outwatch.redux.{Component, Store}

object GenePage extends Component {

  sealed trait Action

  case class Init(geneIRI: IRI) extends Action

  case object ChangeTaxon extends Action

  case class State(geneIRI: IRI) extends ComponentState {

    def evolve: Action => State = {
      case Init(iri) => copy(geneIRI = iri)
    }

  }

  def apply(initState: State): VNode = {
    view(Store.create(Seq.empty, initState))
  }

  def view(store: Store[State, Action]): VNode = {
    import outwatch.dom._

    val obsGeneIRI = store.map(_.geneIRI).distinctUntilChanged
    val obsGene = obsGeneIRI.flatMap(t => KBAPI.gene(t))
    val obsSimilarityComponent = obsGeneIRI.map(iri => GeneTaxonSimilarityPage(GeneTaxonSimilarityPage.State(Some(iri), None)))

    div(
      h2(
        span(cls := "badge", "Gene"), " ",
        span(child <-- obsGene.map(_.label)),
        small(" (", span(child <-- obsGene.map(_.taxon.label)), ")"),
        small(
          " ",
          a(href <-- obsGeneIRI.map(_.id), target := "_blank", cls := "link-no-color",
            child <-- obsGeneIRI.map(iri => Vocab.compact(iri).id)))),
      div(
        cls := "row",
        div(
          cls := "col-sm-12",
          div(
            cls := "top-buffer",
            h3("Data in the Knowledgebase"),
            p(cls := "text-warning", "Under construction"),
            div(child <-- obsSimilarityComponent)
          ))))
  }

}
