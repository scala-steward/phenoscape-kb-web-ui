package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Classification
import org.phenoscape.kb.ui.Model.Taxon
import org.phenoscape.kb.ui.Model.Term
import org.phenoscape.kb.ui.Vocab._

import outwatch.dom._
import outwatch.dom.Attributes.title
import outwatch.dom.VNode
import rxscalajs.Observable

object Views {

  def taxonName(taxon: Taxon): VNode = {
    val isGenusOrSpecies = taxon.rank.map(rank => GenusOrSpecies(rank.iri)).getOrElse(false)
    var classes = List("taxon-name")
    if (taxon.extinct) classes = "extinct" :: classes
    val genusSpecies = if (isGenusOrSpecies) "genus-species" else ""
    span(cls := classes.mkString(" "), title := taxon.iri, span(cls := genusSpecies, taxon.label))
  }

  def classification(data: Classification, termRenderer: Term => VNode): VNode = {
    val superClasses = Observable.of(data.subClassOf.map(term => li(termRenderer(term))))
    val equivalents = Observable.of(data.equivalentTo.map(term => span(" = ", termRenderer(term))))
    val subClasses = Observable.of(data.superClassOf.map(term => li(termRenderer(term))))
    div(
      cls := "classification-level",
      ul(cls := "list-unstyled", children <-- superClasses),
      div(
        cls := "classification-level",
        p(data.label, span(children <-- equivalents)),
        div(cls := "classification-level", ul(cls := "list-unstyled", children <-- subClasses))))
  }

}