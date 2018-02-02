package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.Facet
import org.phenoscape.kb.ui.Model.IRI
import org.phenoscape.kb.ui.Model.Term
import org.phenoscape.kb.ui.Model.TaxonAnnotation

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
    val entityPath = store.map(_.selectedEntityPath).distinctUntilChanged
    val entity = entityPath.map(_.headOption).distinctUntilChanged
    val qualityPath = store.map(_.selectedQualityPath).distinctUntilChanged
    val quality = qualityPath.map(_.headOption).distinctUntilChanged
    val entityTerm = entity.flatMap(e => Util.sequence(e.map(KBAPI.termLabel)))
    val qualityTerm = quality.flatMap(q => Util.sequence(q.map(KBAPI.termLabel)))
    val taxonPath = store.map(_.selectedTaxonPath).distinctUntilChanged
    val taxon = taxonPath.map(_.headOption).distinctUntilChanged
    val taxonTerm = taxon.flatMap(t => Util.sequence(t.map(KBAPI.termLabel)))
    val activeTab = store.map(_.selectedTab).distinctUntilChanged
    val tableTitle = activeTab.map {
      case PhenotypesTab   => "Phenotypes"
      case AnnotationsTab  => "Taxon annotations"
      case TaxaTab         => "Taxa with phenotype"
      case PublicationsTab => "Publications"
    }
    val tableWithData = activeTab.combineLatestWith(entity, quality, taxon)(dataTable)
    val entityCountFn = activeTab.combineLatestWith(quality, taxon) { (tab, q, t) =>
      tab match {
        case PhenotypesTab   => KBAPI.countTaxonPhenotypes(_: Option[IRI], q, t, false, false, false)
        case TaxaTab         => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, false, false, false)
        case AnnotationsTab  => KBAPI.countTaxonAnnotations(_: Option[IRI], q, t, false, false, false)
        case PublicationsTab => KBAPI.countStudiesWithPhenotype(_: Option[IRI], q, t, false, false, false)
      }
    }
    val entityFacetFn = activeTab.combineLatestWith(quality, taxon) { (tab, q, t) =>
      tab match {
        case PhenotypesTab   => KBAPI.facetTaxonPhenotypes("entity", _: Option[IRI], q, t, false, false, false)
        case TaxaTab         => KBAPI.facetTaxaWithPhenotype("entity", _: Option[IRI], q, t, false, false, false)
        case AnnotationsTab  => KBAPI.facetTaxonAnnotations("entity", _: Option[IRI], q, t, false, false, false)
        case PublicationsTab => KBAPI.facetStudiesWithPhenotype("entity", _: Option[IRI], q, t, false, false, false)
      }
    }
    val qualityCountFn = activeTab.combineLatestWith(entity, taxon) { (tab, e, t) =>
      tab match {
        case PhenotypesTab   => KBAPI.countTaxonPhenotypes(e, _: Option[IRI], t, false, false, false)
        case TaxaTab         => KBAPI.countTaxaWithPhenotype(e, _: Option[IRI], t, false, false, false)
        case AnnotationsTab  => KBAPI.countTaxonAnnotations(e, _: Option[IRI], t, false, false, false)
        case PublicationsTab => KBAPI.countStudiesWithPhenotype(e, _: Option[IRI], t, false, false, false)
      }
    }
    val qualityFacetFn = activeTab.combineLatestWith(entity, taxon) { (tab, e, t) =>
      tab match {
        case PhenotypesTab   => KBAPI.facetTaxonPhenotypes("quality", e, _: Option[IRI], t, false, false, false)
        case TaxaTab         => KBAPI.facetTaxaWithPhenotype("quality", e, _: Option[IRI], t, false, false, false)
        case AnnotationsTab  => KBAPI.facetTaxonAnnotations("quality", e, _: Option[IRI], t, false, false, false)
        case PublicationsTab => KBAPI.facetStudiesWithPhenotype("quality", e, _: Option[IRI], t, false, false, false)
      }
    }
    val taxonCountFn = activeTab.combineLatestWith(entity, quality) { (tab, e, q) =>
      tab match {
        case PhenotypesTab   => KBAPI.countTaxonPhenotypes(e, q, _: Option[IRI], false, false, false)
        case TaxaTab         => KBAPI.countTaxaWithPhenotype(e, q, _: Option[IRI], false, false, false)
        case AnnotationsTab  => KBAPI.countTaxonAnnotations(e, q, _: Option[IRI], false, false, false)
        case PublicationsTab => KBAPI.countStudiesWithPhenotype(e, q, _: Option[IRI], false, false, false)
      }
    }
    val taxonFacetFn = activeTab.combineLatestWith(entity, quality) { (tab, e, q) =>
      tab match {
        case PhenotypesTab   => KBAPI.facetTaxonPhenotypes("taxon", e, q, _: Option[IRI], false, false, false)
        case TaxaTab         => KBAPI.facetTaxaWithPhenotype("taxon", e, q, _: Option[IRI], false, false, false)
        case AnnotationsTab  => KBAPI.facetTaxonAnnotations("taxon", e, q, _: Option[IRI], false, false, false)
        case PublicationsTab => KBAPI.facetStudiesWithPhenotype("taxon", e, q, _: Option[IRI], false, false, false)
      }
    }
    val setEntityPath = store.sink.redirectMap(SetEntityPath(_))
    val setQualityPath = store.sink.redirectMap(SetQualityPath(_))
    val setTaxonPath = store.sink.redirectMap(SetTaxonPath(_))
    val entitySearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.Uberon)), 20), entityTerm, (term: Term) => term.label, setEntityPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any anatomical entity"))
    val qualitySearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.PATO)), 20), qualityTerm, (term: Term) => term.label, setQualityPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any phenotypic quality"))
    val taxonSearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.VTO)), 20), taxonTerm, (term: Term) => term.label, setTaxonPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any taxonomic group"))

    div(
      cls := "row",
      div(
        cls := "col-sm-3",
        div(
          cls := "panel panel-default facet-controls",
          style := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", "Query")),
          div(
            cls := "panel-body",
            entitySearch,
            qualitySearch,
            taxonSearch)),
        facetControls("anatomical entity", entityPath, entityCountFn, entityFacetFn, setEntityPath),
        facetControls("phenotypic quality", qualityPath, qualityCountFn, qualityFacetFn, setQualityPath),
        facetControls("taxonomic group", taxonPath, taxonCountFn, taxonFacetFn, setTaxonPath)),
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
            h4(cls := "panel-title", child <-- tableTitle)),
          div(
            cls := "panel-body"),
          child <-- tableWithData)))
  }

  private def dataTable(tab: FacetTab, entity: Option[Model.IRI], quality: Option[Model.IRI], taxon: Option[Model.IRI]): VNode = {
    val (header, rows) = tab match {
      case PhenotypesTab =>
        val data = KBAPI.queryTaxonPhenotypes(entity, quality, taxon, false, false, false).startWith(Nil)
        (
          thead(tr(th("Phenotype"))),
          tbody(children <-- data.map(_.map(singleTermRow))))
      case TaxaTab =>
        val data = KBAPI.queryTaxaWithPhenotype(entity, quality, taxon, false, false, false).startWith(Nil)
        (
          thead(tr(
            th("Group"),
            th("Taxon"))),
          tbody(children <-- data.map(_.map(taxonRow))))
      case AnnotationsTab =>
        val data = KBAPI.queryTaxonAnnotations(entity, quality, taxon, false, false, false).startWith(Nil)
        (
          thead(tr(
            th("Group"),
            th("Taxon"),
            th("Phenotype"),
            th("Source"))),
          tbody(children <-- data.map(_.map(taxonAnnotationRow))))
      case PublicationsTab => //FIXME
        val data = KBAPI.queryStudiesWithPhenotype(entity, quality, taxon, false, false, false).startWith(Nil)
        (
          thead(tr(
            th("Study"))),
          tbody(children <-- data.map(_.map(singleTermRow))))
    }
    table(
      cls := "table table-condensed table-striped", header, rows)
  }

  private def singleTermRow(term: Term): VNode = {
    tr(td(term.label))
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

  private def taxonAnnotationRow(annotation: TaxonAnnotation): VNode = {
    val taxonName = KBAPI.taxon(annotation.taxon.iri).map(Views.taxonName)
    val group = KBAPI.taxonCommonGroup(annotation.taxon.iri).map { grp =>
      val thumbnail = Util.taxonThumbnailIRI(grp.phylopic).id
      span(Popover.simplePopover, cls := "common-taxon-group", data.toggle := "popover", data.trigger := "hover", data.placement := "right", data.content := grp.label,
        img(src := thumbnail))
    } //FIXME repeated code with similarity page
    tr(
      td(child <-- group),
      td(child <-- taxonName.startWith(annotation.taxon.label)),
      td(annotation.phenotype.label),
      td(annotation.source.label))
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