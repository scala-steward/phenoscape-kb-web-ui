package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Facet
import org.phenoscape.kb.ui.Model.IRI

import outwatch.Sink
import outwatch.dom._
import outwatch.dom.VNode
import outwatch.redux.Component
import outwatch.redux.Store
import rxscalajs.Observable

object FacetPage extends Component {

  sealed trait Action
  final case class SelectTab(tab: FacetTab) extends Action
  final case class SetEntityPath(entity: List[IRI]) extends Action
  final case class SetQualityPath(quality: List[IRI]) extends Action
  final case class AddEntityToPath(entity: IRI) extends Action
  final case class AddQualityToPath(quality: IRI) extends Action

  sealed trait FacetTab
  final case object TaxaTab extends FacetTab

  case class State(selectedTab: FacetTab, selectedEntityPath: List[IRI], selectedQualityPath: List[IRI], selectedTaxonPath: List[IRI]) extends ComponentState {

    def evolve = {
      case SelectTab(tab)             => copy(selectedTab = tab)
      case SetEntityPath(entityOpt)   => copy(selectedEntityPath = entityOpt)
      case SetQualityPath(qualityOpt) => copy(selectedQualityPath = qualityOpt)
      case AddEntityToPath(entity)    => copy(selectedEntityPath = entity :: selectedEntityPath)
      case AddQualityToPath(quality)  => copy(selectedQualityPath = quality :: selectedQualityPath)
    }

  }

  def apply(initState: State): VNode = view(Store.create(Seq.empty, initState))

  def view(store: Store[State, Action]): VNode = {
    val entity = store.map(_.selectedEntityPath.headOption).distinctUntilChanged
    val entityPath = store.map(_.selectedEntityPath).distinctUntilChanged
    val quality = store.map(_.selectedQualityPath.headOption).distinctUntilChanged
    val qualityPath = store.map(_.selectedQualityPath).distinctUntilChanged
    val taxon = store.map(_.selectedTaxonPath.headOption).distinctUntilChanged
    val entityCountFn = quality.combineLatestWith(taxon) { (q, t) =>
      KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, false, false, false)
    }
    val entityFacetFn = quality.combineLatestWith(taxon) { (q, t) =>
      KBAPI.facetTaxaWithPhenotype("entity", _: Option[IRI], q, t, false, false, false)
    }
    val qualityCountFn = entity.combineLatestWith(taxon) { (e, t) =>
      KBAPI.countTaxaWithPhenotype(e, _: Option[IRI], t, false, false, false)
    }
    val qualityFacetFn = entity.combineLatestWith(taxon) { (e, t) =>
      KBAPI.facetTaxaWithPhenotype("quality", e, _: Option[IRI], t, false, false, false)
    }
    val setEntityPath = store.sink.redirectMap(SetEntityPath(_))
    val setQualityPath = store.sink.redirectMap(SetQualityPath(_))

    div(
      cls := "row",
      div(
        cls := "col-sm-4",
        facetControls("entity", entityPath, entityCountFn, entityFacetFn, setEntityPath),
        facetControls("quality", qualityPath, qualityCountFn, qualityFacetFn, setQualityPath)),
      div(
        cls := "col-sm-8"))
  }

  type CountFn = Option[IRI] => Observable[Int]

  type FacetFn = Option[IRI] => Observable[List[Facet]]

  case class QuerySpec(entity: Option[IRI])

  def facetControls(title: String, focusItemPath: Observable[List[IRI]], countFunc: Observable[CountFn], facetFunc: Observable[FacetFn], newFocus: Sink[List[IRI]]): VNode = {
    val facetPathPaths = focusItemPath.map { list =>
      list.reverse.scanLeft(List.empty[IRI])((path, term) => term :: path).drop(1)
    }
    val facetPathElements = facetPathPaths.map { paths =>
      val lastIndex = paths.size - 1
      paths.zipWithIndex.map {
        case (path, index) =>
          facetPathLink(path, countFunc, newFocus, index == lastIndex)
      }
    }.startWith(Nil)
    val facetChildElements = focusItemPath.combineLatestWith(facetFunc) { (list, facetFn) =>
      facetFn(list.headOption).startWith(Nil).map(_.sortBy(-_.count).map(facetChildLink(_, list, newFocus)))
    }.flatten
    val anyCount = countFunc.flatMap(_(None).map(_.toString).startWith(""))
    val anySelected = focusItemPath.map(_.isEmpty)
    val anyCSSClass = anySelected.map(flag => if (flag) "selected-facet" else "")
    div(
      div(a(role := "button", cls <-- anyCSSClass, click(Nil) --> newFocus, s"Any $title"), child <-- anyCount),
      div(children <-- facetPathElements),
      div(children <-- facetChildElements))
  }

  private def facetPathLink(termPath: List[IRI], countFunc: Observable[CountFn], newFocus: Sink[List[IRI]], selected: Boolean): VNode = {
    val currentTerm = termPath.head
    val termLabelObs = KBAPI.termLabel(currentTerm).map(_.label)
    val count = countFunc.flatMap(_(Some(currentTerm)))
    val indent = span(termPath.map(_ => span(cls := "facet-indent")): _*)
    val cssClass = if (selected) "selected-facet" else ""
    div(indent, a(role := "button", cls := cssClass, click(termPath) --> newFocus, child <-- termLabelObs), child <-- count)
  }

  private def facetChildLink(facetItem: Facet, termPath: List[IRI], newFocus: Sink[List[IRI]]): VNode = {
    val newPath = facetItem.term.iri :: termPath
    val indent = span(newPath.map(_ => span(cls := "facet-indent")): _*)
    div(indent, a(role := "button", click(newPath) --> newFocus, facetItem.term.label), facetItem.count.toString)
  }

}