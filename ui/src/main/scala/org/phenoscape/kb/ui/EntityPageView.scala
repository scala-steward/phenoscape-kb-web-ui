package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.{EntityPage, FacetPage, Page, PhenotypesTab, PublicationsTab, TaxaTab, TaxonAnnotationsTab}
import org.phenoscape.kb.ui.Model.{HomologyAnnotation, IRI, Relation, Term}
import org.phenoscape.kb.ui.Vocab.{RDFSSubClassOf, Uberon}
import org.phenoscape.kb.ui.components.Popover.popup
import org.phenoscape.kb.ui.components.Views

object EntityPageView {

  def view($state: Signal[EntityPage], updates: WriteBus[Page]): HtmlElement = {
    val $entityIRI = $state.map(_.iri)
    val $termInfo = $entityIRI.flatMap(e => KBAPI.termInfo(e))
    val $homologyAnnotations = $entityIRI.flatMap(e => KBAPI.homologyAnnotations(e))
    val $classificationData = $entityIRI.flatMap(e => KBAPI.classification(e, IRI(Uberon)))
    val relationDLNodes = EventStream.combine($classificationData, $termInfo).map { case (classification, termInfo) =>
      val isaRelations = classification.subClassOf.map(superclass => Relation(Term(IRI(RDFSSubClassOf), "is a type of"), superclass))
      (termInfo.relationships ::: isaRelations).groupBy(_.property).flatMap {
        case (property, relations) =>
          val relationsList = relations.sortBy(_.value.label.toLowerCase).map(relation =>
            a(role := "button", popup := Views.entityInfoView(relation.value.iri, updates), relation.value.label)
          )
          val commaSeparated = relationsList match {
            case first :: rest => rest.foldLeft(List[HtmlElement](first)) { case (list, item) => item :: span(", ") :: list }.reverse
            case Nil           => Nil
          }
          List(dt(property.label, ":"), dd(commaSeparated))
      }.toSeq
    }
    val $taxaLink = $entityIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedEntityPath = List(iri), selectedTab = TaxaTab)) --> updates,
        "Taxa with related phenotypes"
      )
    }
    val $taxonAnnotationsLink = $entityIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedEntityPath = List(iri), selectedTab = TaxonAnnotationsTab)) --> updates,
        "Taxon phenotype annotations"
      )
    }
    val $phenotypesLink = $entityIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedEntityPath = List(iri), selectedTab = PhenotypesTab)) --> updates,
        "Related phenotypes"
      )
    }
    val $pubsLink = $entityIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedEntityPath = List(iri), selectedTab = PublicationsTab)) --> updates,
        "Related publications"
      )
    }

    def termLink(term: Term) = a(
      role := "button",
      onClick.mapTo(EntityPage(term.iri)) --> updates,
      term.label
    )

    div(
      h2(
        span(cls := "badge", "Anatomy"), " ", span(child <-- $termInfo.map(_.term.label)),
        small(
          " ",
          a(href <-- $entityIRI.map(_.id), target := "_blank", cls := "link-no-color",
            child <-- $entityIRI.map(iri => Vocab.compact(iri).id)))),
      div(
        cls := "row",
        div(
          cls := "col-sm-3",
          div(
            cls := "panel panel-default top-buffer",
            div(cls := "panel-body", child <-- $classificationData.map(Views.classification(_, termLink))))),
        div(
          cls := "col-sm-9",
          div(
            cls := "top-buffer",
            h3("Properties"),
            dl(
              dt("Synonyms:"), dd(child <-- $termInfo.map(t => Views.formatSynonyms(t.synonyms))),
              dt("Definition:"), dd(child <-- $termInfo.map(_.definition.map(span(_)).getOrElse(i("None"))))),
            h3("Relationships"),
            dl(children <-- relationDLNodes),
            h4("Homology annotations"),
            div(child <-- $homologyAnnotations.map(as => homologyAnnotationsView(as, updates))),
            h3("Data in the Knowledgebase"),
            p(child <-- $taxaLink),
            p(child <-- $taxonAnnotationsLink),
            p(child <-- $phenotypesLink),
            p(child <-- $pubsLink)
          ))))
  }

  private def homologyAnnotationsView(annotations: List[HomologyAnnotation], updates: WriteBus[Page]): HtmlElement = {
    final case class UnsourcedHomologyAnnotation(subject: IRI, relation: IRI, `object`: IRI, subjectTaxon: IRI, objectTaxon: IRI, negated: Boolean)
    if (annotations.isEmpty) p(i("None"))
    else {
      val grouped = annotations.groupBy {
        case HomologyAnnotation(_, subj, rel, obj, subjectTaxon, objectTaxon, _, negated) => UnsourcedHomologyAnnotation(subj, rel, obj, subjectTaxon, objectTaxon, negated)
      }
      val renderedAnnotations = grouped.map {
        case (UnsourcedHomologyAnnotation(subj, rel, obj, subjectTaxon, objectTaxon, negated), anns) =>
          val subjectLink = a(
            role := "button",
            popup := Views.entityInfoView(subj, updates),
            Views.termName(subj))
          val subjectTaxonLink = a(
            role := "button",
            popup := Views.taxonInfoView(subjectTaxon, updates),
            child <-- KBAPI.taxon(subjectTaxon).map(Views.taxonName)
          )
          val objectLink = a(
            role := "button",
            popup := Views.entityInfoView(obj, updates),
            Views.termName(obj)
          )
          val objectTaxonLink = a(
            role := "button",
            popup := Views.taxonInfoView(objectTaxon, updates),
            child <-- KBAPI.taxon(objectTaxon).map(Views.taxonName)
          )
          val relationLabel = i(Views.termName(rel))
          val negation = if (negated) b(" not ") else span(" ")
          val sources = anns.map { ann =>
            val evidenceLink = a(target := "_blank", href := ann.evidence.id, Views.termName(ann.evidence))
            li(ann.source, ": ", evidenceLink)
          }
          val sourcesPopup = a(
            role := "button",
            popup := ul(cls := "list-unstyled", sources),
            span(cls := "glyphicon glyphicon-list-alt")
          )
          li(subjectLink, " in ", subjectTaxonLink, negation, relationLabel, " ", objectLink, " in ", objectTaxonLink, " ", sourcesPopup)
      }
      ul(cls := "list-unstyled", renderedAnnotations.toList)
    }
  }

}
