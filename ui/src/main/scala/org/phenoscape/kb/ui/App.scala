package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import com.raquo.waypoint._
import org.phenoscape.kb.ui.App.FacetPage.QuerySpec
import org.phenoscape.kb.ui.Model.{Curie, IRI, SimilarityMatch, Term}
import org.scalajs.dom
import upickle.default._
import urldsl.errors.ErrorFromThrowable
import urldsl.vocabulary.{FromString, Printer}

object App {

  sealed trait Page

  case object HomePage extends Page

  case object AboutPage extends Page

  case object OntoTracePage extends Page

  final case class NotFoundPage(path: List[String]) extends Page

  final case class EntityPage(iri: IRI) extends Page

  final case class TaxonPage(iri: IRI) extends Page

  final case class GenePage(iri: IRI) extends Page

  sealed trait FacetTab {
    def key: String
  }

  object FacetTab {

    implicit def tabFromString[A](implicit fromThrowable: ErrorFromThrowable[A]): FromString[FacetTab, A] = FromString.factory { s =>
      Set(TaxaTab, PhenotypesTab, TaxonAnnotationsTab, GenesTab, GeneAnnotationsTab, PublicationsTab).find(_.key == s)
        .toRight(fromThrowable.fromThrowable(new Exception(s"$s is not a facet tab type")))
    }

    implicit def tabPrinter: Printer[FacetTab] = Printer.factory(_.key)

  }

  final case object PhenotypesTab extends FacetTab {
    def key: String = "phenotypes"
  }

  final case object TaxaTab extends FacetTab {
    def key: String = "taxa"
  }

  final case object TaxonAnnotationsTab extends FacetTab {
    def key: String = "taxonannotations"
  }

  final case object GenesTab extends FacetTab {
    def key: String = "genes"
  }

  final case object GeneAnnotationsTab extends FacetTab {
    def key: String = "geneannotations"
  }

  final case object PublicationsTab extends FacetTab {
    def key: String = "publications"
  }

  sealed trait QualityMode {
    def key: String
  }

  object QualityMode {

    implicit def modeFromString[A](implicit fromThrowable: ErrorFromThrowable[A]): FromString[QualityMode, A] = FromString.factory { s =>
      val mode = if (s == InferredPresence.key) InferredPresence
      else if (s == InferredAbsence.key) InferredAbsence
      else PhenotypicQuality
      Right(mode)
    }

    implicit def modePrinter: Printer[QualityMode] = Printer.factory(_.key)

  }

  final case object PhenotypicQuality extends QualityMode {

    def key = "quality"

  }

  final case object InferredPresence extends QualityMode {

    val iri: IRI = IRI("http://purl.org/phenoscape/vocab.owl#inferred_presence")

    def key = "presence"

  }

  final case object InferredAbsence extends QualityMode {

    val iri: IRI = IRI("http://purl.org/phenoscape/vocab.owl#inferred_absence")

    def key = "absence"

  }

  final case class FacetPage(
                              selectedTab: FacetTab = TaxaTab,
                              selectedEntityPath: List[IRI] = Nil,
                              selectedQualityPath: List[IRI] = Nil,
                              selectedTaxonPath: List[IRI] = Nil,
                              selectedPublication: Option[IRI] = None,
                              qualityMode: QualityMode = PhenotypicQuality,
                              includeParts: Boolean = false,
                              includeHistoricalHomologs: Boolean = false,
                              includeSerialHomologs: Boolean = false,
                              taxaPage: Int = 1,
                              phenotypesPage: Int = 1,
                              taxonAnnotationsPage: Int = 1,
                              genesPage: Int = 1,
                              geneAnnotationsPage: Int = 1,
                              publicationsPage: Int = 1) extends Page {

    def currentQuerySpec: QuerySpec = {
      val quality = qualityMode match {
        case PhenotypicQuality => selectedQualityPath.headOption
        case InferredPresence  => Some(InferredPresence.iri)
        case InferredAbsence   => Some(InferredAbsence.iri)
      }
      QuerySpec(selectedEntityPath.headOption, quality, selectedTaxonPath.headOption, selectedPublication, includeParts, includeHistoricalHomologs, includeSerialHomologs)
    }

  }

  object FacetPage {

    final case class QuerySpec(entity: Option[IRI], quality: Option[IRI], taxon: Option[IRI], publication: Option[IRI], includeParts: Boolean, includeHistoricalHomologs: Boolean, includeSerialHomologs: Boolean)

  }

  case class TaxonGeneSimilarityPage(taxonIRIOpt: Option[IRI] = None, selectedPage: Option[Int] = None, selectedMatch: Option[SimilarityMatch] = None) extends Page

  case class GeneTaxonSimilarityPage(geneIRIOpt: Option[IRI] = None, selectedPage: Option[Int] = None, selectedMatch: Option[SimilarityMatch] = None) extends Page

  private implicit val IRIRW: ReadWriter[IRI] = macroRW
  private implicit val EntityPageRW: ReadWriter[EntityPage] = macroRW
  private implicit val TaxonPageRW: ReadWriter[TaxonPage] = macroRW
  private implicit val GenePageRW: ReadWriter[GenePage] = macroRW
  private implicit val QualityModeRW: ReadWriter[QualityMode] = macroRW
  private implicit val FacetTabRW: ReadWriter[FacetTab] = macroRW
  private implicit val FacetPageRW: ReadWriter[FacetPage] = macroRW
  private implicit val TermRW: ReadWriter[Term] = macroRW
  private implicit val SimilarityMatchRW: ReadWriter[SimilarityMatch] = macroRW
  private implicit val TaxonGeneSimilarityPageRW: ReadWriter[TaxonGeneSimilarityPage] = macroRW
  private implicit val GeneTaxonSimilarityPageRW: ReadWriter[GeneTaxonSimilarityPage] = macroRW
  private implicit val NotFoundPageRW: ReadWriter[NotFoundPage] = macroRW
  private implicit val rw: ReadWriter[Page] = macroRW

  implicit def iriFromString[A](implicit fromThrowable: ErrorFromThrowable[A]): FromString[IRI, A] = FromString.factory { iri =>
    Right(Vocab.expand(Curie(iri)))
  }

  implicit def iriPrinter: Printer[IRI] = Printer.factory(iri => Vocab.compact(iri).id)

  implicit def iriListFromString[A](implicit fromThrowable: ErrorFromThrowable[A]): FromString[List[IRI], A] = FromString.factory { list =>
    val curies = list.split(",", -1)
    Right(curies.to(List).map(c => Vocab.expand(Curie(c))))
  }

  implicit def iriListPrinter: Printer[List[IRI]] = Printer.factory(list => list.map(iri => Vocab.compact(iri).id).mkString(","))

  private val homeRoute: Route[HomePage.type, Unit] = Route.static(HomePage, root / endOfSegments)
  private val aboutRoute: Route[AboutPage.type, Unit] = Route.static(AboutPage, root / "about" / "phenoscape" / "kb" / endOfSegments)
  private val ontotraceRoute: Route[OntoTracePage.type, Unit] = Route.static(OntoTracePage, root / "ontotrace" / endOfSegments)
  private val entityRoute: Route[EntityPage, String] = Route(
    encode = p => Vocab.compact(p.iri).id,
    decode = arg => EntityPage(Vocab.expand(Curie(arg))),
    pattern = root / "entity" / segment[String] / endOfSegments
  )
  private val taxonRoute: Route[TaxonPage, String] = Route(
    encode = p => Vocab.compact(p.iri).id,
    decode = arg => TaxonPage(Vocab.expand(Curie(arg))),
    pattern = root / "taxon" / segment[String] / endOfSegments
  )
  private val taxonGeneSimilarityRoute: Route[TaxonGeneSimilarityPage, (Option[IRI], Option[Int])] = Route.onlyQuery(
    encode = p => (p.taxonIRIOpt, p.selectedPage),
    decode = {
      case (taxonIRIOpt, selectedPage) => TaxonGeneSimilarityPage(taxonIRIOpt, selectedPage, selectedMatch = None)
    },
    pattern = (root / "similarity" / "taxon" / endOfSegments) ? (
      param[IRI]("iri").?
        & param[Int]("page").?
      )
  )
  private val geneTaxonSimilarityRoute: Route[GeneTaxonSimilarityPage, (Option[IRI], Option[Int])] = Route.onlyQuery(
    encode = p => (p.geneIRIOpt, p.selectedPage),
    decode = {
      case (geneIRIOpt, selectedPage) => GeneTaxonSimilarityPage(geneIRIOpt, selectedPage, selectedMatch = None)
    },
    pattern = (root / "similarity" / "gene" / endOfSegments) ? (
      param[IRI]("iri").?
        & param[Int]("page").?
      )
  )
  private val facetRoute: Route[FacetPage, (((((((Option[org.phenoscape.kb.ui.App.FacetTab], Option[List[org.phenoscape.kb.ui.Model.IRI]], Option[List[org.phenoscape.kb.ui.Model.IRI]]), Option[List[org.phenoscape.kb.ui.Model.IRI]], Option[org.phenoscape.kb.ui.Model.IRI]), Option[org.phenoscape.kb.ui.App.QualityMode], Option[Boolean]), Option[Boolean], Option[Boolean]), Option[Int], Option[Int]), Option[Int], Option[Int]), Option[Int], Option[Int])] = Route.onlyQuery(
    encode = p => (((((((Some(p.selectedTab), Some(p.selectedEntityPath), Some(p.selectedQualityPath)), Some(p.selectedTaxonPath), p.selectedPublication), Some(p.qualityMode), Some(p.includeParts)), Some(p.includeHistoricalHomologs), Some(p.includeSerialHomologs)), Some(p.taxaPage), Some(p.phenotypesPage)), Some(p.taxonAnnotationsPage), Some(p.genesPage)), Some(p.geneAnnotationsPage), Some(p.publicationsPage)),
    decode = {
      case (((((((tab, entity, quality), taxon, pub), qualityMode, includeParts), includeHistoricalHomologs, includeSerialHomologs), taxaPage, phenotypesPage), taxonAnnotationsPage, genesPage), geneAnnotationsPage, publicationsPage) =>
        FacetPage(selectedTab = tab.getOrElse(TaxaTab),
          selectedEntityPath = entity.getOrElse(Nil),
          selectedQualityPath = quality.getOrElse(Nil),
          selectedTaxonPath = taxon.getOrElse(Nil),
          selectedPublication = pub,
          qualityMode = qualityMode.getOrElse(PhenotypicQuality),
          includeParts = includeParts.getOrElse(false),
          includeHistoricalHomologs = includeHistoricalHomologs.getOrElse(false),
          includeSerialHomologs = includeSerialHomologs.getOrElse(false),
          taxaPage = taxaPage.getOrElse(1),
          phenotypesPage = phenotypesPage.getOrElse(1),
          taxonAnnotationsPage = taxonAnnotationsPage.getOrElse(1),
          genesPage = genesPage.getOrElse(1),
          geneAnnotationsPage = geneAnnotationsPage.getOrElse(1),
          publicationsPage = publicationsPage.getOrElse(1))
    },
    pattern = (root / "facet" / endOfSegments) ? (
      param[FacetTab]("tab").?
        & param[List[IRI]]("entity").?
        & param[List[IRI]]("quality").?
        & param[List[IRI]]("taxon").?
        & param[IRI]("pub").?
        & param[QualityMode]("qualityMode").?
        & param[Boolean]("includeParts").?
        & param[Boolean]("includeHistoricalHomologs").?
        & param[Boolean]("includeSerialHomologs").?
        & param[Int]("taxaPage").?
        & param[Int]("phenotypesPage").?
        & param[Int]("taxaAnnotationsPage").?
        & param[Int]("genesPage").?
        & param[Int]("geneAnnotationsPage").?
        & param[Int]("publicationsPage").?
      )
  )

  private val notFoundRoute: Route[NotFoundPage, List[String]] = Route(
    encode = notFound => notFound.path,
    decode = path => NotFoundPage(path = path),
    pattern = root / remainingSegments / endOfSegments
  )

  private val router = new Router[Page](
    routes = List(entityRoute, taxonRoute, facetRoute, taxonGeneSimilarityRoute, geneTaxonSimilarityRoute, aboutRoute, ontotraceRoute, homeRoute, notFoundRoute),
    getPageTitle = _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage = page => write(page)(rw), // serialize page data for storage in History API log
    deserializePage = pageStr => read(pageStr)(rw) // deserialize the above
  )(
    $popStateEvent = windowEvents.onPopState, // this is how Waypoint avoids an explicit dependency on Laminar
    owner = unsafeWindowOwner, // this router will live as long as the window
  )

  private val updates = new WriteBus[Page]() {
    override def onNext(nextValue: Page): Unit = {
      router.pushState(nextValue)
    }
  }

  private def renderPage($page: Signal[Page]): Div = {
    val appPageSplitter = SplitRender[Page, HtmlElement]($page)
      .collectSignal[EntityPage] { $p => EntityPageView.view($p, updates) }
      .collectSignal[TaxonPage] { $p => TaxonPageView.view($p, updates) }
      .collectSignal[FacetPage] { $p => FacetPageView.view($p, updates) }
      .collectSignal[TaxonGeneSimilarityPage] { $p => TaxonGeneSimilarityPageView.view($p, updates) }
      .collectSignal[GeneTaxonSimilarityPage] { $p => GeneTaxonSimilarityPageView.view($p, updates) }
      .collectStatic(HomePage)(HomePageView.view(updates))
      .collectStatic(OntoTracePage)(OntoTracePageView.view(updates))
      .collectStatic(AboutPage)(AboutPageView.view(updates))
      .collectSignal[NotFoundPage] { $notFound => div("Sorry, nothing to be found here...") }
    BaseLayout.view(updates, appPageSplitter.$view)
  }

  private val appDiv: Div = renderPage(router.$currentPage)

  def main(args: Array[String]): Unit = {
    documentEvents.onDomContentLoaded.foreach { _ =>
      val appContainer = dom.document.querySelector("#appContainer")
      render(appContainer, appDiv)
    }(unsafeWindowOwner)
  }

}
