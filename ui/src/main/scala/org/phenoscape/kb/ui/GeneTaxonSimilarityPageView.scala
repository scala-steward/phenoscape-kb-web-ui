package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.{GeneTaxonSimilarityPage, Page}
import org.phenoscape.kb.ui.Model.{Gene, IRI, SimilarityMatch}
import org.phenoscape.kb.ui.components.Popover.popup
import org.phenoscape.kb.ui.components.Views
import org.phenoscape.kb.ui.components.Views.popoverPanel

object GeneTaxonSimilarityPageView {

  def view($state: Signal[GeneTaxonSimilarityPage], updates: WriteBus[Page]): HtmlElement = {
    val (setGeneIRI, setGeneIRIBinder) = Util.makeAction[Option[IRI], GeneTaxonSimilarityPage]($state, updates) { (geneIRIOpt, page) =>
      page.copy(geneIRIOpt = geneIRIOpt, selectedPage = None, selectedMatch = None)
    }
    val (selectMatchesPage, selectMatchesPageBinder) = Util.makeAction[Int, GeneTaxonSimilarityPage]($state, updates) { (selectedPage, page) =>
      page.copy(selectedPage = Some(selectedPage))
    }
    val (selectMatch, selectMatchBinder) = Util.makeAction[Option[SimilarityMatch], GeneTaxonSimilarityPage]($state, updates) { (selectedMatchOpt, page) =>
      page.copy(selectedMatch = selectedMatchOpt)
    }

    val matchesPageSize = 20
    val $geneIRIOpt = $state.map(_.geneIRIOpt)
    val $page = $state.map(_.selectedPage.getOrElse(1))
    val obsSubject = $geneIRIOpt.flatMap(iriOpt => Util.sequence(iriOpt.map(KBAPI.gene)))
    val corpusSize = KBAPI.similarityCorpusSize(IRI(Vocab.TaxonSimilarityCorpus))
    val similarityMatches = for {
      combined <- $geneIRIOpt.combineWith($page)
      (iriOpt, page) = combined
      offset = (page - 1) * matchesPageSize
      simMatches <- iriOpt.map(iri => KBAPI.similarityMatches(iri, IRI(Vocab.TaxonSimilarityCorpus), matchesPageSize, offset).map(_.results).startWith(Nil)).getOrElse(Signal.fromValue(Nil))
    } yield simMatches
    val obsTotalPages = corpusSize.map(num => (num / matchesPageSize.toDouble).ceil.toInt).startWith(1)
    val $selectedMatch = $state.map(_.selectedMatch)
    val hasMatchSelection = $selectedMatch.map(_.nonEmpty)
    val queryProfileSizeOpt = $geneIRIOpt.flatMap(iriOpt => Util.sequence(iriOpt.map(KBAPI.similarityProfileSize)))
    val selectedMatchProfileSizeOpt = $selectedMatch.flatMap(matchedOpt => Util.sequence(matchedOpt.map(matched => KBAPI.similarityProfileSize(matched.matchProfile.iri))))
    val selectedMatchAsTaxon = $selectedMatch.flatMap(matchedOpt => Util.sequence(matchedOpt.map(matched => KBAPI.taxon(matched.matchProfile.iri))))
    val selectedMatchAnnotations = for {
      combined <- $geneIRIOpt.combineWith($selectedMatch)
      (geneIRIOpt, matchedOpt) = combined
      annotationsOpt <- Util.sequence(for {
        geneIRI <- geneIRIOpt
        matched <- matchedOpt
      } yield KBAPI.bestMatches(geneIRI, IRI(Vocab.GeneSimilarityCorpus), matched.matchProfile.iri, IRI(Vocab.TaxonSimilarityCorpus))).startWith(None)
    } yield annotationsOpt.map(_.results).toList.flatten
    val obsGeneLabel = obsSubject.map(_.map(_.label).getOrElse(""))
    val obsGeneTaxonLabel = obsSubject.map(_.map(_.taxon.label).getOrElse(""))
    val geneSearch = Views.autocompleteField(KBAPI.geneSearch(_: String, 20), obsSubject, (gene: Gene) => s"${gene.label} (${gene.taxon.label})", setGeneIRI.contramap[Option[Gene]](_.map(_.iri)), Some("any gene symbol"), Signal.fromValue(false))

    div(
      setGeneIRIBinder, selectMatchesPageBinder, selectMatchBinder,
      h3("Similar evolutionary variation"),
      p("These taxonomic groups vary in phenotypes that match most closely to the gene profile (collection of phenotypes) that result when the action of this gene is disrupted (e.g., knocked down)."),
      div(
        cls := "panel-body",
        div(
          cls := "row",
          div(
            cls := "col-sm-4",
            h4("Gene:"),
            geneSearch
          )
        ),
        div(
          cls := "row",
          div(
            cls := "col-sm-12",
            h4("Evolutionary profiles matching gene ", b(span(child.text <-- obsGeneLabel)), " (", span(child.text <-- obsGeneTaxonLabel), ")"))),
        div(
          cls := "row",
          div(
            cls := "col-sm-4",
            div(hidden <-- similarityMatches.map(_.nonEmpty), i("No matches")),
            div(
              hidden <-- similarityMatches.map(_.isEmpty),
              table(
                cls := "table table-condensed",
                thead(
                  tr(
                    th("Group"),
                    th("Taxon"),
                    th(
                      popup := "The Expect score is the number of matches one should expect to see at this level of similarity given the size of the database. The lower the Expect score, the more significant the match is.",
                      styleAttr := "white-space: nowrap;", "Expect Score\u00A0", span(cls := "glyphicon glyphicon-info-sign")),
                    th())),
                tbody(children <-- similarityMatches.map(_.map(matchRow(_, $selectedMatch, selectMatch))))),
              Views.pagination($page, selectMatchesPage, obsTotalPages))),
          div(
            cls := "col-sm-8",
            div(
              cls := "well",
              hidden <-- hasMatchSelection,
              h2(cls := "text-center", i(cls := "small", "Click on any row to the left to view detailed results"))),
            div(
              cls := "panel panel-default",
              styleAttr := "margin-top: 1em;",
              hidden <-- hasMatchSelection.map(!_),
              div(
                cls := "panel-heading",
                h4(cls := "panel-title", "Match details")),
              div(
                cls := "panel-body",
                form(
                  cls := "form-horizontal kb-form-condensed",
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Searched gene:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child.text <-- obsGeneLabel), " (", span(child.text <-- obsGeneTaxonLabel), ") ",
                        small(a(
                          //href := "#", //FIXME link to phenotype profile
                          span(child.text <-- queryProfileSizeOpt.map(_.map(_.toString).getOrElse(""))),
                          " phenotypes"))))),
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Matched taxon:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child <-- selectedMatchAsTaxon.map(_.map(Views.taxonName).getOrElse(span()))), " ",
                        small(a(
                          //href := "#", //FIXME link to phenotype profile
                          span(child.text <-- selectedMatchProfileSizeOpt.map(_.map(_.toString).getOrElse(""))),
                          " phenotypes"))))),
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Overall similarity:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child.text <-- $selectedMatch.map(_.map(sm => f"${sm.medianScore}%.2f").getOrElse("")))))),
                  div(
                    cls := "form-group",
                    label(cls := "col-sm-3 control-label", "Expect score:"),
                    div(
                      cls := "col-sm-9",
                      p(
                        cls := "form-control-static",
                        span(child.text <-- $selectedMatch.map(_.map(sm => formatExpect(sm.expectScore)).getOrElse("")))))))),
              table(
                cls := "table table-condensed table-striped",
                thead(
                  tr(
                    th("Gene Phenotype"),
                    th("Taxon Phenotype"),
                    th(
                      styleAttr := "white-space: nowrap;", "Match Information Content\u00A0",
                      popup := "The Match Information Content (IC) describes the specificity of the match between two compared phenotypes. The IC is normalized such that a match with value of 0.0 subsumes all items in the search corpus, while a match with value of 1.0 is annotated to only one item.",
                      span(cls := "glyphicon glyphicon-info-sign")))),
                tbody(children <-- selectedMatchAnnotations.map(_.map(matchAnnotationsRow)))))))))
  }

  private def matchRow(matched: SimilarityMatch, selectedMatch: Signal[Option[Model.SimilarityMatch]], action: Observer[Option[SimilarityMatch]]): HtmlElement = {
    val taxonName = KBAPI.taxon(matched.matchProfile.iri).map(Views.taxonName)
    val hover = Var[Boolean](false)
    val selected = selectedMatch.map(_.contains(matched))
    val hoverOrSelected = hover.signal.combineWithFn(selected)((hov, sel) => hov || sel)
    val group = KBAPI.taxonCommonGroup(matched.matchProfile.iri).map { grp =>
      val thumbnail = Util.taxonThumbnailIRI(grp.phylopic).id
      span(
        popup := grp.label,
        cls := "common-taxon-group",
        img(src := thumbnail)
      )
    }
    tr(
      cls.toggle("active") <-- hover,
      cls.toggle("info") <-- selected,
      onMouseEnter.mapTo(true) --> hover,
      onMouseLeave.mapTo(false) --> hover,
      onClick.mapTo(Some(matched)) --> action,
      td(child <-- group),
      td(child <-- taxonName),
      td(cls := "text-center", a(formatExpect(matched.expectScore))),
      td(cls := "match-list-arrow", span(hidden <-- hoverOrSelected.map(!_), "➠")))
  }

  private def matchAnnotationsRow(annotationMatch: Model.SimilarityAnnotationMatch): HtmlElement = {
    val queryAnnotationTerm = KBAPI.termLabel(annotationMatch.queryAnnotation)
    val corpusAnnotationTerm = KBAPI.termLabel(annotationMatch.corpusAnnotation)
    tr(
      td(child <-- queryAnnotationTerm.map(_.label)),
      td(child <-- corpusAnnotationTerm.map(_.label)),
      td(
        cls := "text-center",
        a(
          role := "button",
          popup := popoverPanel("Match Details")(
            label(forId := "mica", "Most Informative Common Ancestor"),
            p(name := "mica", cls := "form-control-static", span(title := annotationMatch.bestSubsumer.term.iri.id, annotationMatch.bestSubsumer.term.label)),
            label(forId := "mica", "Information Content"),
            p(name := "mica", cls := "form-control-static", span(f"${annotationMatch.bestSubsumer.ic}%.2f"))
          ),
          f"${annotationMatch.bestSubsumer.ic}.2f"), "\u00A0",
        span(hidden := annotationMatch.bestSubsumer.disparity <= 0.25,
          styleAttr := "white-space: nowrap;",
          popup := "This match was found to be informative among genes; however, it is relatively common among taxon annotations.",
          span(cls := "text-danger glyphicon glyphicon-flag")
        )
      )
    )
  }

  private def formatExpect(expectScore: Double): String = if (expectScore < 0.01 && expectScore > -0.01) f"$expectScore%.1E" else f"$expectScore%.2f"

}
