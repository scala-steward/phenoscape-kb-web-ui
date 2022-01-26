package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.{FacetPage, Page, PhenotypesTab, PublicationsTab, TaxaTab, TaxonAnnotationsTab, TaxonPage}
import org.phenoscape.kb.ui.Model.{IRI, Term}
import org.phenoscape.kb.ui.Vocab.VTO
import org.phenoscape.kb.ui.components.Popover.popup
import org.phenoscape.kb.ui.components.Views
import org.phenoscape.kb.ui.components.Views.taxonName

object TaxonPageView {

  def view($state: Signal[TaxonPage], updates: WriteBus[Page]): HtmlElement = {

    val $taxonIRI = $state.map(_.iri)
    val $taxon = $taxonIRI.flatMap(t => KBAPI.taxon(t))
    val $termInfo = $taxonIRI.flatMap(t => KBAPI.termInfo(t))
    val $classificationData = $taxonIRI.flatMap(t => KBAPI.classification(t, IRI(VTO)))

    def taxonTermToView(term: Term) = a(
      role := "button",
      onClick.mapTo(TaxonPage(term.iri)) --> updates,
      child <-- KBAPI.taxon(term.iri).map(Views.taxonName)
    )

    val relationshipsDL = for {
      classification <- $classificationData
    } yield {
      val parents = classification.subClassOf.map { superclass =>
        val taxonNameNode = KBAPI.taxon(superclass.iri).map(Views.taxonName)
        a(role := "button", popup := Views.taxonInfoView(superclass.iri, updates), child <-- taxonNameNode)
      } match {
        case first :: rest => rest.foldLeft(List[HtmlElement](first)) { case (list, item) => item :: span(", ") :: list }.reverse
        case Nil           => Nil
      }
      val children = classification.superClassOf.map { subclass =>
        val taxonNameNode = KBAPI.taxon(subclass.iri).map(Views.taxonName)
        a(role := "button", popup := Views.taxonInfoView(subclass.iri, updates), child <-- taxonNameNode)
      } match {
        case first :: rest => rest.foldLeft(List[HtmlElement](first)) { case (list, item) => item :: span(", ") :: list }.reverse
        case Nil           => Nil
      }
      dl(
        dt("Parent:"),
        dd((if (parents.nonEmpty) parents else List(i("None"))): _*),
        dt("Children:"),
        dd((if (children.nonEmpty) children else List(i("None"))): _*))
    }

    val $taxaLink = $taxonIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedTaxonPath = List(iri), selectedTab = TaxaTab)) --> updates,
        "Taxa in this group with phenotypes"
      )
    }
    val $taxonAnnotationsLink = $taxonIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedTaxonPath = List(iri), selectedTab = TaxonAnnotationsTab)) --> updates,
        "Taxon phenotype annotations"
      )
    }
    val $phenotypesLink = $taxonIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedTaxonPath = List(iri), selectedTab = PhenotypesTab)) --> updates,
        "Phenotypes annotated to taxa within this group"
      )
    }
    val $pubsLink = $taxonIRI.map { iri =>
      a(
        role := "button",
        onClick.mapTo(FacetPage(selectedTaxonPath = List(iri), selectedTab = PublicationsTab)) --> updates,
        "Related publications"
      )
    }

    //    val obsSimilarityComponent = taxonIRIObs.map(iri => TaxonGeneSimilarityPage(TaxonGeneSimilarityPage.State(Some(iri), None)))

    div(
      h2(
        span(cls := "badge", "Taxon"), " ",
        span(child <-- $taxon.map(taxonName)),
        span(hidden := true, hidden <-- $taxon.map(_.commonName.isEmpty), " (", span(child.text <-- $taxon.map(_.commonName.getOrElse(""))), ")"),
        small(
          " ",
          a(href <-- $taxonIRI.map(_.id), target := "_blank", cls := "link-no-color",
            child <-- $taxonIRI.map(iri => Vocab.compact(iri).id)))),
      div(
        cls := "row",
        div(
          cls := "col-sm-3",
          div(
            cls := "panel panel-default top-buffer",
            div(cls := "panel-body", child <-- $classificationData.map(Views.classification(_, taxonTermToView))))),
        div(
          cls := "col-sm-9",
          div(
            cls := "top-buffer",
            h3("Properties"),
            dl(
              dt("Rank:"), dd(child <-- $taxon.map(t => t.rank.map(r => span(r.label)).getOrElse(i("None")))),
              dt("Synonyms:"), dd(child <-- $termInfo.map(t => Views.formatSynonyms(t.synonyms)))),
            h3("Relationships"),
            div(child <-- relationshipsDL),
            h3("Data in the Knowledgebase"),
            p(child <-- $taxaLink),
            p(child <-- $taxonAnnotationsLink),
            p(child <-- $phenotypesLink),
            p(child <-- $pubsLink)
          ))))
  }

}
