package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Classification
import org.phenoscape.kb.ui.Model.Taxon
import org.phenoscape.kb.ui.Model.Term
import org.phenoscape.kb.ui.Vocab._

import outwatch.dom._
import outwatch.dom.Attributes.title
import outwatch.dom.VNode
import rxscalajs.Observable
import outwatch.Sink

object Views {

  def taxonName(taxon: Taxon): VNode = {
    val isGenusOrSpecies = taxon.rank.map(rank => GenusOrSpecies(rank.iri.id)).getOrElse(false)
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

  def pagination(currentPage: Observable[Int], newPage: Sink[Int], totalPages: Observable[Int]): VNode = {
    val onFirstPage = currentPage.map(_ == 1)
    val firstAndPreviousClasses = Util.observableCSS(onFirstPage.map("disabled" -> _))
    val onLastPage = currentPage.combineLatestWith(totalPages)(_ == _)
    val nextAndLastClasses = Util.observableCSS(onLastPage.map("disabled" -> _))
    nav(
      ul(
        cls := "pagination pagination-sm",
        li(cls <-- firstAndPreviousClasses, a(role := "button", click(1) --> newPage, "First")),
        li(cls <-- firstAndPreviousClasses, a(role := "button", click(currentPage.map(_ - 1)) --> newPage, "Previous")),
        li(cls := "active", a(child <-- currentPage)),
        li(cls <-- nextAndLastClasses, a(role := "button", click(currentPage.map(_ + 1)) --> newPage, "Next")),
        li(cls <-- nextAndLastClasses, a(role := "button", click(totalPages) --> newPage, "Last"))))
  }

}