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
import outwatch.dom.VDomModifier.StringNode
import org.phenoscape.kb.ui.App.KBRouter

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
    val obsTermInfo = store.flatMap(s => KBAPI.termInfo(s.taxonIRI))
    val obsClassificationData = store.flatMap(s => KBAPI.classification(s.taxonIRI, IRI(VTO)))
    def taxonTermToView(term: Term) = a(
      href := s"#/taxon/${Vocab.compact(term.iri).id}",
      child <-- KBAPI.taxon(term.iri).map(Views.taxonName))
    val relationshipsDL = for {
      classification <- obsClassificationData
    } yield {
      val parents = Util.interpolate(StringNode(", "), classification.subClassOf.map { superclass =>
        val taxonNameNode = KBAPI.taxon(superclass.iri).map(Views.taxonName).startWith(StringNode(superclass.label))
        Popover.popup(Views.taxonInfoView(superclass.iri), "auto", "focus")(child <-- taxonNameNode)
      })
      val children = Util.interpolate(StringNode(", "), classification.superClassOf.map { subclass =>
        val taxonNameNode = KBAPI.taxon(subclass.iri).map(Views.taxonName).startWith(StringNode(subclass.label))
        Popover.popup(Views.taxonInfoView(subclass.iri), "auto", "focus")(child <-- taxonNameNode)
      })
      dl(
        dt("Parent:"),
        dd((if (parents.nonEmpty) parents else List(i("None"))): _*),
        dt("Children:"),
        dd((if (children.nonEmpty) children else List(i("None"))): _*))
    }
    val res = KBRouter.set(KBRouter.FacetURL)

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
        cls := "row",
        div(
          cls := "col-sm-3",
          div(
            cls := "panel panel-default top-buffer",
            div(cls := "panel-body", child <-- obsClassificationData.map(Views.classification(_, taxonTermToView))))),
        div(
          cls := "col-sm-9",
          div(
            cls := "top-buffer",
            h3("Properties"),
            dl(
              dt("Rank:"), dd(child <-- obsTaxon.map(t => t.rank.map(_.label).getOrElse(i("None")))),
              dt("Synonyms:"), dd(child <-- obsTermInfo.map(t => Views.formatSynonyms(t.synonyms)))),
            h3("Relationships"),
            div(child <-- relationshipsDL),
            h3("Data in the Knowledgebase"),
            a()))))
  }

}