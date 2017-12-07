package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Facet
import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Model.Term

import outwatch.Sink
import outwatch.dom._
import outwatch.dom.Attributes.style
import outwatch.dom.VNode
import outwatch.redux.Component
import outwatch.redux.Store
import rxscalajs.Observable

object FacetPage extends Component {

  sealed trait Action
  final case class SelectTab(tab: FacetTab) extends Action
  final case class SetEntityPath(entity: List[IRI]) extends Action
  final case class SetQualityPath(quality: List[IRI]) extends Action
  final case class SetTaxonPath(quality: List[IRI]) extends Action
  final case class AddEntityToPath(entity: IRI) extends Action
  final case class AddQualityToPath(quality: IRI) extends Action
  final case class AddTaxonToPath(quality: IRI) extends Action
  final case class SetTaxaPage(page: Int) extends Action

  sealed trait FacetTab
  final case object PhenotypesTab extends FacetTab
  final case object TaxaTab extends FacetTab
  final case object AnnotationsTab extends FacetTab
  final case object PublicationsTab extends FacetTab

  case class State(
    selectedTab:         FacetTab,
    selectedEntityPath:  List[IRI],
    selectedQualityPath: List[IRI],
    selectedTaxonPath:   List[IRI],
    taxaPage:            Int       = 1) extends ComponentState {

    def evolve = {
      case SelectTab(tab)              => copy(selectedTab = tab)
      case SetEntityPath(entityList)   => copy(selectedEntityPath = entityList)
      case SetQualityPath(qualityList) => copy(selectedQualityPath = qualityList)
      case SetTaxonPath(taxonList)     => copy(selectedTaxonPath = taxonList)
      case AddEntityToPath(entity)     => copy(selectedEntityPath = entity :: selectedEntityPath)
      case AddQualityToPath(quality)   => copy(selectedQualityPath = quality :: selectedQualityPath)
      case AddTaxonToPath(taxon)       => copy(selectedTaxonPath = taxon :: selectedTaxonPath)
      case SetTaxaPage(page)           => copy(taxaPage = page)
    }

  }

  private type CountFn = Option[IRI] => Observable[Int]

  private type FacetFn = Option[IRI] => Observable[List[Facet]]

  def apply(initState: State): VNode = view(Store.create(Seq.empty, initState))

  def view(store: Store[State, Action]): VNode = {
    val entity = store.map(_.selectedEntityPath.headOption).distinctUntilChanged
    val entityPath = store.map(_.selectedEntityPath).distinctUntilChanged
    val quality = store.map(_.selectedQualityPath.headOption).distinctUntilChanged
    val qualityPath = store.map(_.selectedQualityPath).distinctUntilChanged
    val taxon = store.map(_.selectedTaxonPath.headOption).distinctUntilChanged
    val taxonPath = store.map(_.selectedTaxonPath).distinctUntilChanged
    val activeTab = store.map(_.selectedTab).distinctUntilChanged
    val taxaWithPhenotype = taxon.combineLatestWith(entity, quality) { (t, e, q) =>
      KBAPI.queryTaxaWithPhenotype(e, q, t, false, false, false).startWith(Nil)
    }.flatten
    val entityCountFn = activeTab.combineLatestWith(quality, taxon) { (tab, q, t) =>
      tab match {
        case PhenotypesTab   => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, false, false, false) //FIXME
        case AnnotationsTab  => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, false, false, false) //FIXME
        case TaxaTab         => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, false, false, false)
        case PublicationsTab => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, false, false, false) //FIXME
      }
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
    val taxonCountFn = entity.combineLatestWith(quality) { (e, q) =>
      KBAPI.countTaxaWithPhenotype(e, q, _: Option[IRI], false, false, false)
    }
    val taxonFacetFn = entity.combineLatestWith(quality) { (e, q) =>
      KBAPI.facetTaxaWithPhenotype("taxon", e, q, _: Option[IRI], false, false, false)
    }
    val setEntityPath = store.sink.redirectMap(SetEntityPath(_))
    val setQualityPath = store.sink.redirectMap(SetQualityPath(_))
    val setTaxonPath = store.sink.redirectMap(SetTaxonPath(_))

    div(
      cls := "row",
      div(
        cls := "col-sm-3",
        facetControls("entity", entityPath, entityCountFn, entityFacetFn, setEntityPath),
        facetControls("quality", qualityPath, qualityCountFn, qualityFacetFn, setQualityPath),
        facetControls("taxon", taxonPath, taxonCountFn, taxonFacetFn, setTaxonPath)),
      div(
        cls := "col-sm-9",
        ul(
          cls := "nav nav-tabs",
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == PhenotypesTab))),
            a(click(SelectTab(PhenotypesTab)) --> store, role := "button", "Phenotypes")),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == AnnotationsTab))),
            a(click(SelectTab(AnnotationsTab)) --> store, role := "button", "Annotations")),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == TaxaTab))),
            a(click(SelectTab(TaxaTab)) --> store, role := "button", "Taxa")),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == PublicationsTab))),
            a(click(SelectTab(PublicationsTab)) --> store, role := "button", "Publications"))),
        div(
          cls := "panel panel-default",
          style := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", "Taxa with phenotype")),
          div(
            cls := "panel-body"),
          table(
            cls := "table table-condensed table-striped",
            thead(
              tr(
                th("Group"),
                th("Taxon"))),
            tbody(children <-- taxaWithPhenotype.map(_.map(taxonRow)))))))
  }

  private def taxonRow(term: Term): VNode = {
    val taxonName = KBAPI.taxon(term.iri).map(Views.taxonName)
    val group = KBAPI.taxonCommonGroup(term.iri).map { grp =>
      val thumbnail = Util.taxonThumbnailIRI(grp.phylopic).id
      span(Popover.simplePopover, cls := "common-taxon-group", data.toggle := "popover", data.trigger := "hover", data.placement := "right", data.content := grp.label,
        img(src := thumbnail))
    } //FIXME repeated code with similarity page
    tr(
      td(child <-- group),
      td(child <-- taxonName.startWith(term.label)))
  }

  private def facetControls(title: String, focusItemPath: Observable[List[IRI]], countFunc: Observable[CountFn], facetFunc: Observable[FacetFn], newFocus: Sink[List[IRI]]): VNode = {
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
      cls := "panel panel-default facet-controls",
      style := "margin-top: 1em;",
      div(
        cls := "panel-heading",
        h4(cls := "panel-title", title.capitalize)),
      div(
        cls := "panel-body",
        div(a(role := "button", cls <-- anyCSSClass, click(Nil) --> newFocus, s"Any $title"), " ", span(cls := "badge", child <-- anyCount)),
        div(children <-- facetPathElements),
        div(children <-- facetChildElements)))
  }

  private def facetPathLink(termPath: List[IRI], countFunc: Observable[CountFn], newFocus: Sink[List[IRI]], selected: Boolean): VNode = {
    val currentTerm = termPath.head
    val termLabelObs = KBAPI.termLabel(currentTerm).map(_.label)
    val count = countFunc.flatMap(_(Some(currentTerm)).map(_.toString).startWith(""))
    val indent = span(termPath.map(_ => span(cls := "facet-indent")): _*)
    val cssClass = if (selected) "selected-facet" else ""
    div(indent, a(role := "button", cls := cssClass, click(termPath) --> newFocus, child <-- termLabelObs), " ", span(cls := "badge", child <-- count))
  }

  private def facetChildLink(facetItem: Facet, termPath: List[IRI], newFocus: Sink[List[IRI]]): VNode = {
    val newPath = facetItem.term.iri :: termPath
    val indent = span(newPath.map(_ => span(cls := "facet-indent")): _*)
    div(indent, a(role := "button", click(newPath) --> newFocus, facetItem.term.label), " ", span(cls := "badge", facetItem.count.toString))
  }

}