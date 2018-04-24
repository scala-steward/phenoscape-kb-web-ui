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
import outwatch.dom.helpers.EventEmitterBuilder
import scala.annotation.tailrec

object FacetPage extends Component {

  sealed trait Action
  final case class SelectTab(tab: FacetTab) extends Action
  final case class SetEntityPath(entity: List[IRI]) extends Action
  final case class SetQualityPath(quality: List[IRI]) extends Action
  final case class SetTaxonPath(taxon: List[IRI]) extends Action
  final case class SetPublication(publication: Option[IRI]) extends Action
  final case class AddEntityToPath(entity: IRI) extends Action
  final case class AddQualityToPath(quality: IRI) extends Action
  final case class AddTaxonToPath(quality: IRI) extends Action
  final case class SetPage(tab: FacetTab, page: Int) extends Action

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
    selectedPublication: Option[IRI],
    taxaPage:            Int         = 1,
    phenotypesPage:      Int         = 1,
    annotationsPage:     Int         = 1,
    publicationsPage:    Int         = 1) extends ComponentState {

    def evolve = {
      case SelectTab(tab)              => copy(selectedTab = tab)
      case SetEntityPath(entityList)   => copy(selectedEntityPath = entityList, taxaPage = 1, phenotypesPage = 1, annotationsPage = 1, publicationsPage = 1)
      case SetQualityPath(qualityList) => copy(selectedQualityPath = qualityList, taxaPage = 1, phenotypesPage = 1, annotationsPage = 1, publicationsPage = 1)
      case SetTaxonPath(taxonList)     => copy(selectedTaxonPath = taxonList, taxaPage = 1, phenotypesPage = 1, annotationsPage = 1, publicationsPage = 1)
      case SetPublication(pubOpt)      => copy(selectedPublication = pubOpt, taxaPage = 1, phenotypesPage = 1, annotationsPage = 1, publicationsPage = 1)
      case AddEntityToPath(entity)     => copy(selectedEntityPath = entity :: selectedEntityPath, taxaPage = 1, phenotypesPage = 1, annotationsPage = 1, publicationsPage = 1)
      case AddQualityToPath(quality)   => copy(selectedQualityPath = quality :: selectedQualityPath, taxaPage = 1, phenotypesPage = 1, annotationsPage = 1, publicationsPage = 1)
      case AddTaxonToPath(taxon)       => copy(selectedTaxonPath = taxon :: selectedTaxonPath, taxaPage = 1, phenotypesPage = 1, annotationsPage = 1, publicationsPage = 1)
      case SetPage(tab, page) => tab match {
        case PhenotypesTab   => copy(phenotypesPage = page)
        case TaxaTab         => copy(taxaPage = page)
        case AnnotationsTab  => copy(annotationsPage = page)
        case PublicationsTab => copy(publicationsPage = page)
      }
    }

  }

  private type CountFn = Option[IRI] => Observable[Int]

  private type FacetFn = Option[IRI] => Observable[List[Facet]]

  def apply(initState: State): VNode = view(Store.create(Seq.empty, initState))

  def view(store: Store[State, Action]): VNode = {
    val tablePageSize = 40
    val entityPath = store.map(_.selectedEntityPath).distinctUntilChanged
    val entity = entityPath.map(_.headOption).distinctUntilChanged
    val qualityPath = store.map(_.selectedQualityPath).distinctUntilChanged
    val quality = qualityPath.map(_.headOption).distinctUntilChanged
    val entityTerm = entity.flatMap(e => Util.sequence(e.map(KBAPI.termLabel)))
    val qualityTerm = quality.flatMap(q => Util.sequence(q.map(KBAPI.termLabel)))
    val taxonPath = store.map(_.selectedTaxonPath).distinctUntilChanged
    val taxon = taxonPath.map(_.headOption).distinctUntilChanged
    val taxonTerm = taxon.flatMap(t => Util.sequence(t.map(KBAPI.termLabel)))
    val publication = store.map(_.selectedPublication).distinctUntilChanged
    val publicationTerm = publication.flatMap(p => Util.sequence(p.map(KBAPI.termLabel)))
    val activeTab = store.map(_.selectedTab).distinctUntilChanged
    val obsTaxonPage = store.map(_.taxaPage).distinctUntilChanged
    val obsPhenotypesPage = store.map(_.phenotypesPage).distinctUntilChanged
    val obsAnnotationsPage = store.map(_.annotationsPage).distinctUntilChanged
    val obsPublicationsPage = store.map(_.publicationsPage).distinctUntilChanged
    val tableTitle = activeTab.map {
      case PhenotypesTab   => "Phenotypes"
      case AnnotationsTab  => "Taxon annotations"
      case TaxaTab         => "Taxa with phenotype"
      case PublicationsTab => "Publications"
    }
    def totalToPages(num: Int): Int = (num / tablePageSize.toDouble).ceil.toInt
    val querySpecObs = entity.combineLatest(quality, taxon, publication)
    val phenotypesTotalObs = querySpecObs.flatMap {
      case (e, q, t, p) =>
        KBAPI.countTaxonPhenotypes(e, q, t, p, false, false, false).map(Option(_)).startWith(None)
    }
    val phenotypesTotalPagesObs = phenotypesTotalObs.map(_.map(totalToPages))
    val taxaTotalObs = querySpecObs.flatMap {
      case (e, q, t, p) =>
        KBAPI.countTaxaWithPhenotype(e, q, t, p, false, false, false).map(Option(_)).startWith(None)
    }
    val taxaTotalPagesObs = taxaTotalObs.map(_.map(totalToPages))
    val annotationsTotalObs = querySpecObs.flatMap {
      case (e, q, t, p) =>
        KBAPI.countTaxonAnnotations(e, q, t, p, false, false, false).map(Option(_)).startWith(None)
    }
    val annotationsTotalPagesObs = annotationsTotalObs.map(_.map(totalToPages))
    val publicationsTotalObs = querySpecObs.flatMap {
      case (e, q, t, p) =>
        KBAPI.countStudiesWithPhenotype(e, q, t, p, false, false, false).map(Option(_)).startWith(None)
    }
    val publicationsTotalPagesObs = publicationsTotalObs.map(_.map(totalToPages))
    val currentPagesObs = store.map(s => Map[FacetTab, Int](
      PhenotypesTab -> s.phenotypesPage,
      TaxaTab -> s.taxaPage,
      AnnotationsTab -> s.annotationsPage,
      PublicationsTab -> s.publicationsPage)).distinctUntilChanged
    val tableWithData = activeTab.combineLatestWith(querySpecObs, currentPagesObs)(dataTable(_, _, _, tablePageSize))
    val entityCountFn = activeTab.combineLatestWith(quality, taxon, publication) { (tab, q, t, p) =>
      tab match {
        case PhenotypesTab   => KBAPI.countTaxonPhenotypes(_: Option[IRI], q, t, p, false, false, false)
        case TaxaTab         => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, p, false, false, false)
        case AnnotationsTab  => KBAPI.countTaxonAnnotations(_: Option[IRI], q, t, p, false, false, false)
        case PublicationsTab => KBAPI.countStudiesWithPhenotype(_: Option[IRI], q, t, p, false, false, false)
      }
    }
    val entityFacetFn = activeTab.combineLatestWith(quality, taxon, publication) { (tab, q, t, p) =>
      tab match {
        case PhenotypesTab   => KBAPI.facetTaxonPhenotypes("entity", _: Option[IRI], q, t, p, false, false, false)
        case TaxaTab         => KBAPI.facetTaxaWithPhenotype("entity", _: Option[IRI], q, t, p, false, false, false)
        case AnnotationsTab  => KBAPI.facetTaxonAnnotations("entity", _: Option[IRI], q, t, p, false, false, false)
        case PublicationsTab => KBAPI.facetStudiesWithPhenotype("entity", _: Option[IRI], q, t, p, false, false, false)
      }
    }
    val qualityCountFn = activeTab.combineLatestWith(entity, taxon, publication) { (tab, e, t, p) =>
      tab match {
        case PhenotypesTab   => KBAPI.countTaxonPhenotypes(e, _: Option[IRI], t, p, false, false, false)
        case TaxaTab         => KBAPI.countTaxaWithPhenotype(e, _: Option[IRI], t, p, false, false, false)
        case AnnotationsTab  => KBAPI.countTaxonAnnotations(e, _: Option[IRI], t, p, false, false, false)
        case PublicationsTab => KBAPI.countStudiesWithPhenotype(e, _: Option[IRI], t, p, false, false, false)
      }
    }
    val qualityFacetFn = activeTab.combineLatestWith(entity, taxon, publication) { (tab, e, t, p) =>
      tab match {
        case PhenotypesTab   => KBAPI.facetTaxonPhenotypes("quality", e, _: Option[IRI], t, p, false, false, false)
        case TaxaTab         => KBAPI.facetTaxaWithPhenotype("quality", e, _: Option[IRI], t, p, false, false, false)
        case AnnotationsTab  => KBAPI.facetTaxonAnnotations("quality", e, _: Option[IRI], t, p, false, false, false)
        case PublicationsTab => KBAPI.facetStudiesWithPhenotype("quality", e, _: Option[IRI], t, p, false, false, false)
      }
    }
    val taxonCountFn = activeTab.combineLatestWith(entity, quality, publication) { (tab, e, q, p) =>
      tab match {
        case PhenotypesTab   => KBAPI.countTaxonPhenotypes(e, q, _: Option[IRI], p, false, false, false)
        case TaxaTab         => KBAPI.countTaxaWithPhenotype(e, q, _: Option[IRI], p, false, false, false)
        case AnnotationsTab  => KBAPI.countTaxonAnnotations(e, q, _: Option[IRI], p, false, false, false)
        case PublicationsTab => KBAPI.countStudiesWithPhenotype(e, q, _: Option[IRI], p, false, false, false)
      }
    }
    val taxonFacetFn = activeTab.combineLatestWith(entity, quality, publication) { (tab, e, q, p) =>
      tab match {
        case PhenotypesTab   => KBAPI.facetTaxonPhenotypes("taxon", e, q, _: Option[IRI], p, false, false, false)
        case TaxaTab         => KBAPI.facetTaxaWithPhenotype("taxon", e, q, _: Option[IRI], p, false, false, false)
        case AnnotationsTab  => KBAPI.facetTaxonAnnotations("taxon", e, q, _: Option[IRI], p, false, false, false)
        case PublicationsTab => KBAPI.facetStudiesWithPhenotype("taxon", e, q, _: Option[IRI], p, false, false, false)
      }
    }
    val setEntityPath = store.sink.redirectMap(SetEntityPath(_))
    val setQualityPath = store.sink.redirectMap(SetQualityPath(_))
    val setTaxonPath = store.sink.redirectMap(SetTaxonPath(_))
    val setPublicationOpt = store.sink.redirectMap(SetPublication(_))
    val entitySearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.Uberon)), 20), entityTerm, (term: Term) => term.label, setEntityPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any anatomical entity"))
    val qualitySearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.PATO)), 20), qualityTerm, (term: Term) => term.label, setQualityPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any phenotypic quality"))
    val taxonSearch = Views.autocompleteField(KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.VTO)), 20), taxonTerm, (term: Term) => term.label, setTaxonPath.redirectMap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any taxonomic group"))
    val pubSearch = Views.autocompleteField(KBAPI.studySearch, publicationTerm, (term: Term) => term.label, setPublicationOpt.redirectMap((ot: Option[Term]) => ot.map(_.iri)), Some("any publication"))

    div(
      cls := "row",
      div(
        cls := "col-sm-4",
        div(
          cls := "panel panel-default facet-controls query-panel",
          style := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", "Query")),
          div(
            cls := "panel-body",
            entitySearch,
            qualitySearch,
            taxonSearch,
            pubSearch)),
        facetControls("anatomical entity", entityPath, entityCountFn, entityFacetFn, setEntityPath),
        facetControls("phenotypic quality", qualityPath, qualityCountFn, qualityFacetFn, setQualityPath),
        facetControls("taxonomic group", taxonPath, taxonCountFn, taxonFacetFn, setTaxonPath)),
      div(
        cls := "col-sm-8",
        ul(
          cls := "nav nav-tabs",
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == PhenotypesTab))),
            a(click(SelectTab(PhenotypesTab)) --> store, role := "button", "Phenotypes ", span(cls := "badge", child <-- phenotypesTotalObs.map(_.getOrElse(""))))),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == AnnotationsTab))),
            a(click(SelectTab(AnnotationsTab)) --> store, role := "button", "Annotations ", span(cls := "badge", child <-- annotationsTotalObs.map(_.getOrElse(""))))),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == TaxaTab))),
            a(click(SelectTab(TaxaTab)) --> store, role := "button", "Taxa ", span(cls := "badge", child <-- taxaTotalObs.map(_.getOrElse(""))))),
          li(role := "presentation", cls <-- Util.observableCSS(activeTab.map(t => "active" -> (t == PublicationsTab))),
            a(click(SelectTab(PublicationsTab)) --> store, role := "button", "Publications ", span(cls := "badge", child <-- publicationsTotalObs.map(_.getOrElse("")))))),
        div(
          cls := "panel panel-default",
          style := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", child <-- tableTitle)),
          div(
            cls := "panel-body"),
          child <-- tableWithData),
        div(hidden <-- activeTab.map(_ != TaxaTab), Views.pagination(obsTaxonPage, store.redirectMap(SetPage(TaxaTab, _)), taxaTotalPagesObs.map(_.getOrElse(0)))),
        div(hidden <-- activeTab.map(_ != PhenotypesTab), Views.pagination(obsPhenotypesPage, store.redirectMap(SetPage(PhenotypesTab, _)), phenotypesTotalPagesObs.map(_.getOrElse(0)))),
        div(hidden <-- activeTab.map(_ != AnnotationsTab), Views.pagination(obsAnnotationsPage, store.redirectMap(SetPage(AnnotationsTab, _)), annotationsTotalPagesObs.map(_.getOrElse(0)))),
        div(hidden <-- activeTab.map(_ != PublicationsTab), Views.pagination(obsPublicationsPage, store.redirectMap(SetPage(PublicationsTab, _)), publicationsTotalPagesObs.map(_.getOrElse(0))))))
  }

  private def dataTable(tab: FacetTab, querySpec: (Option[Model.IRI], Option[Model.IRI], Option[Model.IRI], Option[Model.IRI]), currentPages: Map[FacetTab, Int], tableSize: Int): VNode = {
    val (entity, quality, taxon, publication) = querySpec
    def offset(page: Int) = tableSize * ((page - 1).max(0))
    val (header, rows) = tab match {
      case PhenotypesTab =>
        val data = KBAPI.queryTaxonPhenotypes(entity, quality, taxon, publication, false, false, false, tableSize, offset(currentPages(PhenotypesTab))).startWith(Nil)
        (
          thead(tr(th("Phenotype"))),
          tbody(children <-- data.map(_.map(singleTermRow))))
      case TaxaTab =>
        val data = KBAPI.queryTaxaWithPhenotype(entity, quality, taxon, publication, false, false, false, tableSize, offset(currentPages(TaxaTab))).startWith(Nil)
        (
          thead(tr(
            th("Group"),
            th("Taxon"))),
          tbody(children <-- data.map(_.map(taxonRow))))
      case AnnotationsTab =>
        val data = KBAPI.queryTaxonAnnotations(entity, quality, taxon, publication, false, false, false, tableSize, offset(currentPages(AnnotationsTab))).startWith(Nil)
        (
          thead(tr(
            th("Group"),
            th("Taxon"),
            th("Phenotype"),
            th("Source"))),
          tbody(children <-- data.map(_.map(taxonAnnotationRow))))
      case PublicationsTab =>
        val data = KBAPI.queryStudiesWithPhenotype(entity, quality, taxon, publication, false, false, false, tableSize, offset(currentPages(PublicationsTab))).startWith(Nil)
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
        div(cls := "facet-line", a(role := "button", cls <-- anyCSSClass, click(Nil) --> newFocus, s"Any $title"), " ", span(cls := "badge", child <-- anyCount)),
        div(children <-- facetPathElements),
        div(children <-- facetChildElements)))
  }

  private def facetPathLink(termPath: List[IRI], countFunc: Observable[CountFn], newFocus: Sink[List[IRI]], selected: Boolean): VNode = {
    val currentTerm = termPath.head
    val termLabelObs = KBAPI.termLabel(currentTerm).map(_.label)
    val count = countFunc.flatMap(_(Some(currentTerm)).map(_.toString).startWith(""))
    val indent = span(termPath.map(_ => span(cls := "facet-indent")): _*)
    val cssClass = if (selected) "selected-facet" else ""
    val showPopup = createBoolHandler(false) //FIXME clean up into popover API
    div(cls := "facet-line", indent,
      Popover.popup(termInfoView(currentTerm), "right")(cls := cssClass, click(termPath) --> newFocus, child <-- termLabelObs),
      " ", span(cls := "badge", child <-- count))
  }

  private def facetChildLink(facetItem: Facet, termPath: List[IRI], newFocus: Sink[List[IRI]]): VNode = {
    val newPath = facetItem.term.iri :: termPath
    val indent = span(newPath.map(_ => span(cls := "facet-indent")): _*)
    div(cls := "facet-line", indent,
      Popover.popup(termInfoView(facetItem.term.iri), "right")(click(newPath) --> newFocus, facetItem.term.label),
      " ", span(cls := "badge", facetItem.count.toString))
  }

  private def termInfoView(iri: IRI): VNode = {
    val term = KBAPI.termInfo(iri)
    def formatSynonyms(syns: List[(IRI, String)]): VNode = if (syns.isEmpty) i("None")
    else {
      val synNodes = syns.sortBy(_._2.toLowerCase).map { case (relation, value) => span(value, " ", span(cls := "synonym-type", s"(${Vocab.synonymTypes(relation)})")) }
      span(interpolate(span(", "), synNodes): _*)
    }
    div(
      h4(child <-- term.map(_.term.label)),
      dl(
        dt("Synonyms"), dd(child <-- term.map(t => formatSynonyms(t.synonyms))),
        dt("Definition"), dd(child <-- term.map(_.definition.getOrElse(i("None")))),
        dt("ID"), dd(Vocab.compact(iri).id)))
  }

  private def interpolate[T](elem: T, xs: List[T]): List[T] = xs match {
    case Nil             => Nil
    case last @ x :: Nil => last
    case x :: xs         => x :: elem :: interpolate(elem, xs)
  }

}