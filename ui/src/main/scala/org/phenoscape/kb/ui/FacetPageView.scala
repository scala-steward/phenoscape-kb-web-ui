package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.FacetPage.QuerySpec
import org.phenoscape.kb.ui.App.{FacetPage, FacetTab, GeneAnnotationsTab, GenePage, GenesTab, InferredAbsence, InferredPresence, Page, PhenotypesTab, PhenotypicQuality, PublicationsTab, QualityMode, TaxaTab, TaxonAnnotationsTab}
import org.phenoscape.kb.ui.Model._
import org.phenoscape.kb.ui.components.Popover.popup
import org.phenoscape.kb.ui.components.{Popover, Views}
import sttp.model.Uri

object FacetPageView {

  private type CountFn = Option[IRI] => EventStream[Int]

  private type FacetFn = Option[IRI] => EventStream[List[Facet]]

  def view($state: Signal[FacetPage], updates: WriteBus[Page]): HtmlElement = {
    val tablePageSize = 40
    val $entityPath = $state.map(_.selectedEntityPath)
    val $querySpec = $state.map(_.currentQuerySpec)
    val $entity = $querySpec.map(_.entity)
    val $qualityPath = $state.map(_.selectedQualityPath)
    val $quality = $querySpec.map(_.quality)
    val $entityTerm = $entity.flatMap(e => Util.sequence(e.map(KBAPI.termLabel)))
    val $qualityTerm = $quality.flatMap {
      case Some(InferredPresence.iri) => EventStream.fromValue(Some(Term(InferredPresence.iri, "Inferred Presence")))
      case Some(InferredAbsence.iri)  => EventStream.fromValue(Some(Term(InferredAbsence.iri, "Inferred Absence")))
      case q                          => Util.sequence(q.map(KBAPI.termLabel))
    }
    val $qualityMode = $state.map(_.qualityMode)
    val $taxonPath = $state.map(_.selectedTaxonPath)
    val $taxon = $querySpec.map(_.taxon)
    val $taxonTerm = $taxon.flatMap(t => Util.sequence(t.map(KBAPI.termLabel)))
    val $publication = $querySpec.map(_.publication)
    val $publicationTerm = $publication.flatMap(p => Util.sequence(p.map(KBAPI.termLabel)))
    val $activeTab = $state.map(_.selectedTab)
    val $taxonPage = $state.map(_.taxaPage)
    val $phenotypesPage = $state.map(_.phenotypesPage)
    val $annotationsPage = $state.map(_.taxonAnnotationsPage)
    val $publicationsPage = $state.map(_.publicationsPage)
    val $genePage = $state.map(_.genesPage)
    val $tableTitle = $activeTab.map {
      case PhenotypesTab       => span("A feature of an organism and its quality, annotated with ontology terms")
      case TaxonAnnotationsTab => span("Associations of a phenotype with a taxon")
      case TaxaTab             => span("Taxa associated with phenotypes in the KB")
      case GeneAnnotationsTab  => span("Associations of a phenotype with a gene")
      case GenesTab            => span("Genes associated with phenotypes in the KB")
      case PublicationsTab     => span("Studies that have been annotated in the KB")
    }

    def totalToPages(num: Int): Int = (num / tablePageSize.toDouble).ceil.toInt

    val $phenotypesTotal = $querySpec.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countTaxonPhenotypes(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val $phenotypesTotalPages = $phenotypesTotal.map(_.map(totalToPages))
    val $taxaTotal = $querySpec.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countTaxaWithPhenotype(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val $taxaTotalPages = $taxaTotal.map(_.map(totalToPages))
    val $annotationsTotal = $querySpec.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countTaxonAnnotations(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val $annotationsTotalPages = $annotationsTotal.map(_.map(totalToPages))
    val $publicationsTotal = $querySpec.flatMap {
      case QuerySpec(e, q, t, p, parts, hist, serial) =>
        KBAPI.countStudiesWithPhenotype(e, q, t, p, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val $publicationsTotalPages = $publicationsTotal.map(_.map(totalToPages))
    val $genesTotal = $querySpec.flatMap {
      case QuerySpec(e, q, _, _, parts, hist, serial) =>
        KBAPI.countGenesWithPhenotype(e, q, parts, hist, serial).map(Option(_)).startWith(None)
    }
    val $genesTotalPages = $genesTotal.map(_.map(totalToPages))
    val $currentPages = $state.map(s => Map[FacetTab, Int](
      PhenotypesTab -> s.phenotypesPage,
      TaxaTab -> s.taxaPage,
      TaxonAnnotationsTab -> s.taxonAnnotationsPage,
      PublicationsTab -> s.publicationsPage,
      GenesTab -> s.genesPage,
      GeneAnnotationsTab -> s.geneAnnotationsPage))
    val $downloadURLandTable = Signal.combineWithFn($activeTab, $querySpec, $currentPages) {
      dataTable(_, _, _, tablePageSize, updates)
    }
    val $downloadURL = $downloadURLandTable.map(_._1)
    val $tableWithData = $downloadURLandTable.map(_._2)
    val entityCountFn = Signal.combineWithFn($querySpec.map(spec => (spec.quality, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)), $activeTab) {
      case ((q, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.countTaxonPhenotypes(_: Option[IRI], q, t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.countTaxaWithPhenotype(_: Option[IRI], q, t, p, parts, hist, serial)
          case GenesTab            => KBAPI.countGenesWithPhenotype(_: Option[IRI], q, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.countTaxonAnnotations(_: Option[IRI], q, t, p, parts, hist, serial)
          //          case GeneAnnotationsTab  => KBAPI.countGeneAnnotations()
          case PublicationsTab => KBAPI.countStudiesWithPhenotype(_: Option[IRI], q, t, p, parts, hist, serial)
        }
    }
    val entityFacetFn = Signal.combineWithFn($querySpec.map(spec => (spec.quality, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)), $activeTab) {
      case ((q, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.facetTaxonPhenotypes("entity", _: Option[IRI], q, t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.facetTaxaWithPhenotype("entity", _: Option[IRI], q, t, p, parts, hist, serial)
          case GenesTab            => KBAPI.facetGenesWithPhenotype("entity", _: Option[IRI], q, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.facetTaxonAnnotations("entity", _: Option[IRI], q, t, p, parts, hist, serial)
          case PublicationsTab     => KBAPI.facetStudiesWithPhenotype("entity", _: Option[IRI], q, t, p, parts, hist, serial)
        }
    }
    val qualityCountFn = Signal.combineWithFn($querySpec.map(spec => (spec.entity, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)), $activeTab) {
      case ((e, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.countTaxonPhenotypes(e, _: Option[IRI], t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.countTaxaWithPhenotype(e, _: Option[IRI], t, p, parts, hist, serial)
          case GenesTab            => KBAPI.countGenesWithPhenotype(e, _: Option[IRI], parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.countTaxonAnnotations(e, _: Option[IRI], t, p, parts, hist, serial)
          case PublicationsTab     => KBAPI.countStudiesWithPhenotype(e, _: Option[IRI], t, p, parts, hist, serial)
        }
    }
    val qualityFacetFn = Signal.combineWithFn($querySpec.map(spec => (spec.entity, spec.taxon, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)), $activeTab) {
      case ((e, t, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.facetTaxonPhenotypes("quality", e, _: Option[IRI], t, p, parts, hist, serial)
          case TaxaTab             => KBAPI.facetTaxaWithPhenotype("quality", e, _: Option[IRI], t, p, parts, hist, serial)
          case GenesTab            => KBAPI.facetGenesWithPhenotype("quality", e, _: Option[IRI], parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.facetTaxonAnnotations("quality", e, _: Option[IRI], t, p, parts, hist, serial)
          case PublicationsTab     => KBAPI.facetStudiesWithPhenotype("quality", e, _: Option[IRI], t, p, parts, hist, serial)
        }
    }
    val taxonCountFn = Signal.combineWithFn($querySpec.map(spec => (spec.entity, spec.quality, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)), $activeTab) {
      case ((e, q, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.countTaxonPhenotypes(e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxaTab             => KBAPI.countTaxaWithPhenotype(e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.countTaxonAnnotations(e, q, _: Option[IRI], p, parts, hist, serial)
          case PublicationsTab     => KBAPI.countStudiesWithPhenotype(e, q, _: Option[IRI], p, parts, hist, serial)
          case GenesTab            => (_: Option[IRI]) => EventStream.fromValue(0)
        }
    }
    val taxonFacetFn = Signal.combineWithFn($querySpec.map(spec => (spec.entity, spec.quality, spec.publication, spec.includeParts, spec.includeHistoricalHomologs, spec.includeSerialHomologs)), $activeTab) {
      case ((e, q, p, parts, hist, serial), tab) =>
        tab match {
          case PhenotypesTab       => KBAPI.facetTaxonPhenotypes("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxaTab             => KBAPI.facetTaxaWithPhenotype("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case TaxonAnnotationsTab => KBAPI.facetTaxonAnnotations("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case PublicationsTab     => KBAPI.facetStudiesWithPhenotype("taxon", e, q, _: Option[IRI], p, parts, hist, serial)
          case GenesTab            => (_: Option[IRI]) => EventStream.fromValue(Nil)
        }
    }

    //      case AddEntityToPath(entity)               => copy(selectedEntityPath = entity :: selectedEntityPath, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    //      case AddQualityToPath(quality)             => copy(qualityMode = PhenotypicQuality, selectedQualityPath = quality :: selectedQualityPath, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    //      case AddTaxonToPath(taxon)                 => copy(selectedTaxonPath = taxon :: selectedTaxonPath, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    //      case SetIncludeParts(include)              =>
    //      case SetIncludeHistoricalHomologs(include) =>
    //      case SetIncludeSerialHomologs(include)     =>
    //      case SetQualityMode(mode)                  =>
    //      case SetPage(tab, page)                    => tab match {
    //        case PhenotypesTab       => copy(phenotypesPage = page)
    //        case TaxaTab             => copy(taxaPage = page)
    //        case TaxonAnnotationsTab => copy(taxonAnnotationsPage = page)
    //        case GenesTab            => copy(genesPage = page)
    //        case GeneAnnotationsTab  => copy(geneAnnotationsPage = page)
    //        case PublicationsTab     => copy(publicationsPage = page)
    //      }
    //
    //  }
    val (selectTab, selectTabBinder) = Util.makeAction[FacetTab, FacetPage]($state, updates) { (tab, page) =>
      page.copy(selectedTab = tab)
    }
    val (setEntityPath, setEntityPathBinder) = Util.makeAction[List[IRI], FacetPage]($state, updates) { (iris, page) =>
      page.copy(selectedEntityPath = iris, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setQualityPath, setQualityPathBinder) = Util.makeAction[List[IRI], FacetPage]($state, updates) { (iris, page) =>
      page.copy(qualityMode = PhenotypicQuality, selectedQualityPath = iris, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setTaxonPath, setTaxonPathBinder) = Util.makeAction[List[IRI], FacetPage]($state, updates) { (iris, page) =>
      page.copy(selectedTaxonPath = iris, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setPublicationOpt, setPublicationOptBinder) = Util.makeAction[Option[IRI], FacetPage]($state, updates) { (pubOpt, page) =>
      page.copy(selectedPublication = pubOpt, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setIncludeParts, setIncludePartsBinder) = Util.makeAction[Boolean, FacetPage]($state, updates) { (include, page) =>
      page.copy(includeParts = include, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setIncludeHistoricalHomologs, setIncludeHistoricalHomologsBinder) = Util.makeAction[Boolean, FacetPage]($state, updates) { (include, page) =>
      page.copy(includeHistoricalHomologs = include, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setIncludeSerialHomologs, setIncludeSerialHomologsBinder) = Util.makeAction[Boolean, FacetPage]($state, updates) { (include, page) =>
      page.copy(includeSerialHomologs = include, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setQualityMode, setQualityModeBinder) = Util.makeAction[QualityMode, FacetPage]($state, updates) { (mode, page) =>
      page.copy(qualityMode = mode, taxaPage = 1, phenotypesPage = 1, taxonAnnotationsPage = 1, genesPage = 1, geneAnnotationsPage = 1, publicationsPage = 1)
    }
    val (setPage, setPageBinder) = Util.makeAction[(FacetTab, Int), FacetPage]($state, updates) { case ((tab, pageNum), page) =>
      tab match {
        case PhenotypesTab       => page.copy(phenotypesPage = pageNum)
        case TaxaTab             => page.copy(taxaPage = pageNum)
        case TaxonAnnotationsTab => page.copy(taxonAnnotationsPage = pageNum)
        case GenesTab            => page.copy(genesPage = pageNum)
        case GeneAnnotationsTab  => page.copy(geneAnnotationsPage = pageNum)
        case PublicationsTab     => page.copy(publicationsPage = pageNum)
      }
    }
    val entitySearch = Views.autocompleteField[Term](KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.Uberon)), 20), $entityTerm, (term: Term) => term.label, setEntityPath.contramap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any anatomical entity"), EventStream.fromValue(false))
    val qualitySearch = Views.autocompleteField[Term](KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.PATO)), 20), $qualityTerm, (term: Term) => term.label, setQualityPath.contramap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any phenotypic quality"), $qualityMode.map(_ != PhenotypicQuality))
    val taxonSearch = Views.autocompleteField[Term](KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.VTO)), 20), $taxonTerm, (term: Term) => term.label, setTaxonPath.contramap((ot: Option[Term]) => ot.map(_.iri).toList), Some("any taxonomic group"), EventStream.fromValue(false))
    val pubSearch = Views.autocompleteField[Term](KBAPI.studySearch, $publicationTerm, (term: Term) => term.label, setPublicationOpt.contramap((ot: Option[Term]) => ot.map(_.iri)), Some("any publication"), EventStream.fromValue(false))

    div(
      selectTabBinder, setEntityPathBinder, setQualityPathBinder, setTaxonPathBinder, setPublicationOptBinder, setIncludePartsBinder, setIncludeHistoricalHomologsBinder, setIncludeSerialHomologsBinder, setQualityModeBinder, setPageBinder,
      cls := "row",
      div(
        cls := "col-sm-4",
        div(
          cls := "panel panel-default facet-controls query-panel",
          styleAttr := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", "Query")),
          div(
            cls := "panel-body",
            entitySearch,
            qualitySearch,
            taxonSearch,
            pubSearch)),
        facetControls("anatomical entity", $entityPath, entityCountFn, entityFacetFn, setEntityPath, EventStream.fromValue(false), EventStream.fromValue(false))(Some(div(
          cls := "form-inline",
          div(
            div(cls := "form-group", label("Include: ")),
            div(cls := "checkbox",
              label(
                input(
                  tpe := "checkbox",
                  checked <-- $state.map(_.includeParts),
                  onChange.mapToChecked --> setIncludeParts),
                " parts ")),
            div(cls := "checkbox",
              label(
                input(
                  tpe := "checkbox",
                  checked <-- $state.map(_.includeHistoricalHomologs),
                  onChange.mapToChecked --> setIncludeHistoricalHomologs),
                " historical homologs ")),
            div(cls := "checkbox",
              label(
                input(
                  tpe := "checkbox",
                  checked <-- $state.map(_.includeSerialHomologs),
                  onChange.mapToChecked --> setIncludeSerialHomologs),
                " serial homologs ")))))),
        facetControls("phenotypic quality", $qualityPath, qualityCountFn, qualityFacetFn, setQualityPath, EventStream.fromValue(false), $qualityMode.map(_ != PhenotypicQuality))(Some(div(
          cls := "form-inline",
          div(
            div(cls := "form-group", label("Mode: ")),
            div(cls := "radio",
              label(
                input(
                  tpe := "radio",
                  name := "phenotypic-quality-radio",
                  checked <-- $qualityMode.map(_ == PhenotypicQuality),
                  onChange.mapToChecked.filter(_ == true).mapTo(PhenotypicQuality) --> setQualityMode),
                " quality ")),
            div(cls := "radio",
              label(
                input(
                  tpe := "radio",
                  name := "phenotypic-quality-radio",
                  checked <-- $qualityMode.map(_ == InferredPresence),
                  onChange.mapToChecked.filter(_ == true).mapTo(InferredPresence) --> setQualityMode),
                " inferred presence ")),
            div(cls := "radio",
              label(
                input(
                  tpe := "radio",
                  name := "phenotypic-quality-radio",
                  checked <-- $qualityMode.map(_ == InferredAbsence),
                  onChange.mapToChecked.filter(_ == true).mapTo(InferredAbsence) --> setQualityMode),
                " inferred absence ")))))),
        facetControls("taxonomic group", $taxonPath, taxonCountFn, taxonFacetFn, setTaxonPath, $activeTab.map(t => Set[FacetTab](GenesTab, GeneAnnotationsTab)(t)), EventStream.fromValue(false))(None)),
      div(
        cls := "col-sm-8",
        ul(
          cls := "nav nav-tabs",
          li(role := "presentation", cls.toggle("active") <-- $activeTab.map(_ == PhenotypesTab),
            a(onClick.mapTo(PhenotypesTab) --> selectTab, role := "button", "Phenotypes ", span(cls := "badge", child.text <-- $phenotypesTotal.map(_.map(_.toString).getOrElse(""))))),
          li(role := "presentation", cls.toggle("active") <-- $activeTab.map(_ == TaxonAnnotationsTab),
            a(onClick.mapTo(TaxonAnnotationsTab) --> selectTab, role := "button", "Taxon annotations ", span(cls := "badge", child.text <-- $annotationsTotal.map(_.map(_.toString).getOrElse(""))))),
          li(role := "presentation", cls.toggle("active") <-- $activeTab.map(_ == TaxaTab),
            a(onClick.mapTo(TaxaTab) --> selectTab, role := "button", "Taxa ", span(cls := "badge", child.text <-- $taxaTotal.map(_.map(_.toString).getOrElse(""))))),
          li(role := "presentation", cls.toggle("active") <-- $activeTab.map(_ == PublicationsTab),
            a(onClick.mapTo(PublicationsTab) --> selectTab, role := "button", "Publications ", span(cls := "badge", child.text <-- $publicationsTotal.map(_.map(_.toString).getOrElse(""))))),
          li(role := "presentation", cls.toggle("active") <-- $activeTab.map(_ == GenesTab),
            a(onClick.mapTo(GenesTab) --> selectTab, role := "button", "Genes ", span(cls := "badge", child.text <-- $genesTotal.map(_.map(_.toString).getOrElse("")))))),
        div(
          cls := "panel panel-default",
          styleAttr := "margin-top: 1em;",
          div(
            cls := "panel-heading",
            h4(cls := "panel-title", child <-- $tableTitle)),
          div(
            cls := "panel-body",
            a(href <-- $downloadURL.map(_.toString()), target := "_blank", span(cls := "glyphicon glyphicon-download-alt"), " Download data as text")),
          child <-- $tableWithData),
        div(hidden <-- $activeTab.map(_ != TaxaTab), Views.pagination($taxonPage, setPage.contramap((TaxaTab, _)), $taxaTotalPages.map(_.getOrElse(0)))),
        div(hidden <-- $activeTab.map(_ != PhenotypesTab), Views.pagination($phenotypesPage, setPage.contramap((PhenotypesTab, _)), $phenotypesTotalPages.map(_.getOrElse(0)))),
        div(hidden <-- $activeTab.map(_ != TaxonAnnotationsTab), Views.pagination($annotationsPage, setPage.contramap((TaxonAnnotationsTab, _)), $annotationsTotalPages.map(_.getOrElse(0)))),
        div(hidden <-- $activeTab.map(_ != PublicationsTab), Views.pagination($publicationsPage, setPage.contramap((PublicationsTab, _)), $publicationsTotalPages.map(_.getOrElse(0)))),
        div(hidden <-- $activeTab.map(_ != GenesTab), Views.pagination($genePage, setPage.contramap((GenesTab, _)), $genesTotalPages.map(_.getOrElse(0)))))
    )
  }

  private def dataTable(tab: FacetTab, querySpec: QuerySpec, currentPages: Map[FacetTab, Int], tableSize: Int, updates: WriteBus[Page]): (Uri, HtmlElement) = {
    val QuerySpec(entity, quality, taxon, publication, parts, hist, serial) = querySpec

    def offset(page: Int) = tableSize * (page - 1).max(0)

    val (url, header, rows) = tab match {
      case PhenotypesTab       =>
        val url = KBAPI.queryTaxonPhenotypesURL(entity, quality, taxon, publication, parts, hist, serial, 0, 0)
        val data = KBAPI.queryTaxonPhenotypes(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(PhenotypesTab))).startWith(Nil)
        (
          url,
          thead(tr(th("Phenotype"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(singleTermRow))))
      case TaxaTab             =>
        val url = KBAPI.queryTaxaWithPhenotypeURL(entity, quality, taxon, publication, parts, hist, serial, 0, 0)
        val data = KBAPI.queryTaxaWithPhenotype(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(TaxaTab))).startWith(Nil)
        (
          url,
          thead(tr(
            th("Group"),
            th("Taxon"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(t => taxonRow(t, updates)))))
      case TaxonAnnotationsTab =>
        val url = KBAPI.queryTaxonAnnotationsURL(entity, quality, taxon, publication, parts, hist, serial, 0, 0)
        val data = KBAPI.queryTaxonAnnotations(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(TaxonAnnotationsTab))).startWith(Nil)
        (
          url,
          thead(tr(
            th("Group"),
            th("Taxon"),
            th("Phenotype"),
            th("Sources"))),
          tbody(
            cls := "sibling-striped",
            children <-- data.map(_.flatMap(ta => taxonAnnotationRow(ta, updates)))))
      case PublicationsTab     =>
        val url = KBAPI.queryStudiesWithPhenotypeURL(entity, quality, taxon, publication, parts, hist, serial, 0, 0)
        val data = KBAPI.queryStudiesWithPhenotype(entity, quality, taxon, publication, parts, hist, serial, tableSize, offset(currentPages(PublicationsTab))).startWith(Nil)
        (
          url,
          thead(tr(
            th("Publication"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(publicationRow))))
      case GenesTab            =>
        val url = KBAPI.queryGenesWithPhenotypeURL(entity, quality, parts, hist, serial, 0, 0)
        val data = KBAPI.queryGenesWithPhenotype(entity, quality, parts, hist, serial, tableSize, offset(currentPages(GenesTab))).startWith(Nil)
        (
          url,
          thead(tr(
            th("Organism"),
            th("Gene"))),
          tbody(
            cls := "striped",
            children <-- data.map(_.map(t => geneRow(t, updates)))))
      case GeneAnnotationsTab  => ???
    }
    url -> table(cls := "table table-condensed", header, rows)
  }

  private def singleTermRow(term: Term): HtmlElement = {
    tr(td(term.label))
  }

  private def taxonRow(term: Term, updates: WriteBus[Page]): HtmlElement = {
    val taxonName = KBAPI.taxon(term.iri).map(Views.taxonName)
    val group = KBAPI.taxonCommonGroup(term.iri).map { grp =>
      val thumbnail = Util.taxonThumbnailIRI(grp.phylopic).id
      span(cls := "common-taxon-group", popup := grp.label, img(src := thumbnail))
    } //FIXME repeated code with similarity page
    tr(
      cls := "striped",
      td(child <-- group),
      td(a(role := "button", popup := Views.taxonInfoView(term.iri, updates), child <-- taxonName.startWith(term.label)))
    )
  }

  private def taxonAnnotationRow(annotation: TaxonAnnotation, updates: WriteBus[Page]): Seq[HtmlElement] = {
    val taxonName = KBAPI.taxon(annotation.taxon.iri).map(Views.taxonName)
    val group = KBAPI.taxonCommonGroup(annotation.taxon.iri).map { grp =>
      val thumbnail = Util.taxonThumbnailIRI(grp.phylopic).id
      span(cls := "common-taxon-group", popup := grp.label, img(src := thumbnail))
    } //FIXME repeated code with similarity page
    val siblingToggleClickBus = new EventBus[Unit]()
    val innerView = siblingToggleClickBus.events.map(_ => KBAPI.annotationSources(annotation).map(_.map(annotationSourceView))).flatten
    val shouldHideSiblingView = siblingToggleClickBus.events.foldLeft(0)((a, _) => a + 1).map(i => (i % 2) == 0)
    Seq(
      tr(
        cls := "striped",
        td(child <-- group),
        td(a(role := "button", popup := Views.taxonInfoView(annotation.taxon.iri, updates), child <-- taxonName.startWith(annotation.taxon.label))),
        td(annotation.phenotype.label),
        td(a(
          role := "button",
          onClick.mapTo(()) --> siblingToggleClickBus,
          span(cls := "glyphicon glyphicon-list-alt")))),
      tr(
        hidden <-- shouldHideSiblingView,
        td(
          colSpan := 4,
          div(
            cls := "well",
            h4("Source data"),
            div(children <-- innerView)))))
  }

  private def annotationSourceView(as: AnnotationSource): HtmlElement = {
    div(
      h5(a(role := "button", popup := Views.publicationInfoView(as.study.iri), as.study.label)),
      dl(
        cls := "dl-horizontal",
        dt(s"Character ${as.characterNum}:"),
        dd(as.characterText),
        dt("State:"),
        dd(as.stateText)))
  }

  private def geneRow(term: Term, updates: WriteBus[Page]): HtmlElement = {
    val thumbnail = Util.modelOrganismThumbnailURL(term.iri)
    tr(
      td(span(cls := "common-taxon-group", img(src := thumbnail))),
      td(a(
        role := "button",
        onClick.mapTo(GenePage(term.iri)) --> updates,
        term.label)))
  }

  private def publicationRow(term: Term): HtmlElement = tr(td(a(role := "button", popup := Views.publicationInfoView(term.iri), term.label)))

  private def facetControls(title: String, focusItemPath: Signal[List[IRI]], countFunc: Signal[CountFn], facetFunc: Signal[FacetFn], newFocus: Observer[List[IRI]], disabledObs: Observable[Boolean], facetsHiddenObs: Observable[Boolean])(accessoryView: Option[HtmlElement]): HtmlElement = {
    val facetPathPaths = focusItemPath.map { list =>
      list.reverse.scanLeft(List.empty[IRI])((path, term) => term :: path).drop(1)
    }
    val facetPathElements = facetPathPaths.map { paths =>
      val lastIndex = paths.size - 1
      paths.zipWithIndex.map {
        case (path, index) =>
          facetPathLink(path, countFunc, newFocus, index == lastIndex)
      }
    } //.startWith(Nil)
    val facetDataAndStatus = focusItemPath.combineWithFn(facetFunc) { (list, facetFn) =>
      facetFn(list.headOption).map(x => (x, list) -> true).startWith((Nil, list) -> false)
    }.flatten
    val facetChildElements = facetDataAndStatus.map {
      case ((facets, iris), _) =>
        facets.sortBy(-_.count).map(facetChildLink(_, iris, newFocus))
    }
    val childrenLoaded = facetDataAndStatus.map(_._2)
    val anyCount = countFunc.flatMap(_ (None).map(_.toString).startWith(""))
    val anySelected = focusItemPath.map(_.isEmpty)
    val anyCSSClass = anySelected.map(flag => if (flag) "selected-facet" else "")

    div(
      cls := "panel panel-default facet-controls",
      styleAttr := "margin-top: 1em;",
      div(
        cls := "panel-heading",
        h4(cls := "panel-title", title.capitalize)),
      div(
        hidden <-- disabledObs,
        cls := "panel-body",
        div(cls := "facet-accessory", accessoryView.getOrElse(div())),
        div(
          hidden <-- facetsHiddenObs,
          div(cls := "facet-line", a(role := "button", cls <-- anyCSSClass, onClick.mapTo(Nil) --> newFocus, s"Any $title"), " ", span(cls := "badge", child.text <-- anyCount)),
          div(children <-- facetPathElements),
          div(children <-- facetChildElements),
          div(hidden <-- childrenLoaded, Views.loading))))
  }

  private def facetPathLink(termPath: List[IRI], countFunc: Signal[CountFn], newFocus: Observer[List[IRI]], selected: Boolean): HtmlElement = {
    val currentTerm = termPath.head
    val termLabelObs = KBAPI.termLabel(currentTerm).map(_.label)
    val countValue = countFunc.flatMap(_ (Some(currentTerm)))
    val countLoaded = countValue.map(_ => true).startWith(false)
    val count = countValue.map(_.toString).startWith("")
    val indent = span(termPath.map(_ => span(cls := "facet-indent")): _*)
    val cssClass = if (selected) "selected-facet" else ""
    div(cls := "facet-line", indent,
      a(role := "button", cls := cssClass, popup := Views.termInfoView(currentTerm), onClick.mapTo(termPath) --> newFocus, child.text <-- termLabelObs),
      " ", span(hidden <-- countLoaded, Views.loading), span(cls := "badge", child.text <-- count))
  }

  private def facetChildLink(facetItem: Facet, termPath: List[IRI], newFocus: Observer[List[IRI]]): HtmlElement = {
    val newPath = facetItem.term.iri :: termPath
    val indent = span(newPath.map(_ => span(cls := "facet-indent")): _*)
    div(cls := "facet-line", indent,
      a(role := "button", popup := Views.termInfoView(facetItem.term.iri), onClick.mapTo(newPath) --> newFocus, facetItem.term.label),
      " ", span(cls := "badge", facetItem.count.toString))
  }

}
