package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Classification
import org.phenoscape.kb.ui.Model.Taxon
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
    if (isGenusOrSpecies) classes = "genus-species" :: classes
    span(cls := classes.mkString(" "), title := taxon.iri, taxon.label)
  }

  def classification(data: Classification): VNode = {
    val superClasses = Observable.of(data.subClassOf.map(term => li(term.label)))
    val equivalents = Observable.of(data.equivalentTo.map(term => span(" = ", term.label)))
    val subClasses = Observable.of(data.superClassOf.map(term => li(term.label)))
    div(
      cls := "classification-level",
      ul(cls := "list-unstyled", children <-- superClasses),
      div(
        cls := "classification-level",
        p(data.label, span(children <-- equivalents)),
        div(cls := "classification-level", ul(cls := "list-unstyled", children <-- subClasses))))
  }

}