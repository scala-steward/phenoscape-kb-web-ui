package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.App.KBRouter.FacetURLP
import org.phenoscape.kb.ui.Model.{HomologyAnnotation, IRI, Relation, Term}
import org.phenoscape.kb.ui.Vocab._
import outwatch.dom.VDomModifier.StringNode
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
    val entityIRIObs = store.map(_.entityIRI).distinctUntilChanged
    val obsTermInfo: Observable[Model.TermInfo] = entityIRIObs.flatMap(e => KBAPI.termInfo(e))
    val obsHomologyAnnotations = entityIRIObs.flatMap(e => KBAPI.homologyAnnotations(e))
    val obsClassificationData = entityIRIObs.flatMap(e => KBAPI.classification(e, IRI(Uberon)))
    val relationDLNodes = for {
      classification <- obsClassificationData
      termInfo <- obsTermInfo
    } yield {
      val isaRelations = classification.subClassOf.map(superclass => Relation(Term(IRI(RDFSSubClassOf), "is a type of"), superclass))
      (termInfo.relationships ::: isaRelations).groupBy(_.property).flatMap {
        case (property, relations) =>
          val relationsList = Util.interpolate(StringNode(", "), relations.sortBy(_.value.label.toLowerCase).map(relation =>
            Popover.popup(Views.entityInfoView(relation.value.iri), "auto", "focus")(relation.value.label)))
          List(dt(property.label, ":"), dd(relationsList: _*))
      }.toSeq
    }
    val taxaLink = entityIRIObs.map(e => FacetURLP.urlForState(FacetPage.State(FacetPage.TaxaTab, List(e), Nil, Nil, None, false, false, false)))
    val taxonAnnotationsLink = entityIRIObs.map(e => FacetURLP.urlForState(FacetPage.State(FacetPage.TaxonAnnotationsTab, List(e), Nil, Nil, None, false, false, false)))
    val phenotypesLink = entityIRIObs.map(e => FacetURLP.urlForState(FacetPage.State(FacetPage.PhenotypesTab, List(e), Nil, Nil, None, false, false, false)))
    val pubsLink = entityIRIObs.map(e => FacetURLP.urlForState(FacetPage.State(FacetPage.PublicationsTab, List(e), Nil, Nil, None, false, false, false)))

    def termLink(term: Term) = a(href := s"#/entity/${Vocab.compact(term.iri).id}", term.label)

    div(
      h2(
        span(cls := "badge", "Anatomy"), " ",
        span(child <-- obsTermInfo.map(_.term.label)),
        small(
          " ",
          a(href <-- store.map(_.entityIRI.id), target := "_blank", cls := "link-no-color",
            child <-- store.map(s => Vocab.compact(s.entityIRI).id)))),
      div(
        cls := "row",
        div(
          cls := "col-sm-3",
          div(
            cls := "panel panel-default top-buffer",
            div(cls := "panel-body", child <-- obsClassificationData.map(Views.classification(_, termLink))))),
        div(
          cls := "col-sm-9",
          div(
            cls := "top-buffer",
            h3("Properties"),
            dl(
              dt("Synonyms:"), dd(child <-- obsTermInfo.map(t => Views.formatSynonyms(t.synonyms))),
              dt("Definition:"), dd(child <-- obsTermInfo.map(_.definition.getOrElse(i("None"))))),
            h3("Relationships"),
            dl(children <-- relationDLNodes),
            h4("Homology annotations"),
            div(child <-- obsHomologyAnnotations.map(homologyAnnotationsView)),
            h3("Data in the Knowledgebase"),
            p(a(href <-- taxaLink, "Taxa with related phenotypes")),
            p(a(href <-- taxonAnnotationsLink, "Taxon phenotype annotations")),
            p(a(href <-- phenotypesLink, "Related phenotypes")),
            p(a(href <-- pubsLink, "Related publications"))))))
  }
  
  private def homologyAnnotationsView(annotations: List[HomologyAnnotation]): VNode = {
    import outwatch.dom._
    if (annotations.isEmpty) p(i("None"))
    else {
      def annotation(ann: HomologyAnnotation): VNode = {
        val subjectLink = a(href := Util.linkToEntity(ann.subject), Views.termName(ann.subject))
        val subjectTaxonLink = a(href := Util.linkToTaxon(ann.subjectTaxon), child <-- KBAPI.taxon(ann.subjectTaxon).map(Views.taxonName))
        val objectLink = a(href := Util.linkToEntity(ann.`object`), Views.termName(ann.`object`))
        val objectTaxonLink = a(href := Util.linkToTaxon(ann.objectTaxon), child <-- KBAPI.taxon(ann.objectTaxon).map(Views.taxonName))
        val relationLabel = i(Views.termName(ann.relation))
        val evidenceLink = a(target := "_blank", href := ann.evidence.id, Views.termName(ann.evidence))
        li(subjectLink, " in ", subjectTaxonLink, " ", relationLabel, " ", objectLink, " in ", objectTaxonLink, " (", ann.source, ": ", evidenceLink, ")")
      }

      val sorted = annotations.sortBy(_.relation.id)
      ul(cls := "list-unstyled", children <-- Observable.of(sorted.map(annotation)))
    }
  }

}
