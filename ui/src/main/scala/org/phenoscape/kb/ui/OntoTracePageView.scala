package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.Page
import org.phenoscape.kb.ui.Model.{IRI, Term}
import org.phenoscape.kb.ui.Util.StringOps
import org.phenoscape.kb.ui.components.Views

object OntoTracePageView {

  sealed trait InputMode

  final case object SimpleMode extends InputMode

  final case object ExpressionMode extends InputMode

  def view(updates: WriteBus[Page]): HtmlElement = {
    val $mode = Var[InputMode](SimpleMode)
    val $taxonIRIOpt = Var[Option[IRI]](None)
    val $entityIRIOpt = Var[Option[IRI]](None)
    val $includeParts = Var[Boolean](false)
    val $includeAllCharacters = Var[Boolean](false)
    val $taxonExpression = Var[Either[Option[String], String]](Left(None))
    val $entityExpression = Var[Either[Option[String], String]](Left(None))
    val $showSimpleMode = $mode.signal.map(_ == SimpleMode)
    val $includePartsProcessed = $showSimpleMode.combineWithFn($includeParts)(_ && _)
    val $simpleTaxonTerm = $taxonIRIOpt.signal.flatMap(iriOpt => Util.sequence(iriOpt.map(KBAPI.termLabel)))
    val $simpleEntityTerm = $entityIRIOpt.signal.flatMap(iriOpt => Util.sequence(iriOpt.map(KBAPI.termLabel)))
    val $ontotraceDownloadURL = $mode.signal.combineWith($taxonIRIOpt.signal, $entityIRIOpt.signal, $taxonExpression.signal, $entityExpression.signal, $includePartsProcessed, $includeAllCharacters.signal)
      .map { case (mode, taxonIRIOpt, entityIRIOpt, taxonExpression, entityExpression, includeParts, includeAllCharacters) =>
        val (taxonOpt, entityOpt) = mode match {
          case SimpleMode     => (taxonIRIOpt.map(iri => s"<${iri.id}>"), entityIRIOpt.map(iri => s"<${iri.id}>"))
          case ExpressionMode => (taxonExpression.toOption, entityExpression.toOption)
        }
        for {
          taxon <- taxonOpt
          entity <- entityOpt
        } yield KBAPI.ontotraceURL(taxon, entity, includeParts, !includeAllCharacters)
      }
    val $invalidTaxonError = $taxonExpression.signal.map(_.swap.toOption.flatten)
    val $hasTaxonExpressionFeedback = $taxonExpression.signal.map {
      case Right(_)      => true
      case Left(Some(_)) => true
      case Left(None)    => false
    }
    val $hasValidTaxonExpression = $taxonExpression.signal.map(_.isRight)
    val $enterRawTaxonExpression = new EventBus[String]()
    val $resolvedTaxonExpression = for {
      raw <- $enterRawTaxonExpression.events.delay(500)
      maybeText = raw.stripToOption
      maybeResolved <- Util.sequence(maybeText.map(KBAPI.resolveLabelExpression))
    } yield maybeResolved match {
      case Some(result) => result.left.map(Option(_))
      case None         => Left(None)
    }
    val $invalidEntityError = $entityExpression.signal.map(_.swap.toOption.flatten)
    val $hasEntityExpressionFeedback = $entityExpression.signal.map {
      case Right(_)      => true
      case Left(Some(_)) => true
      case Left(None)    => false
    }
    val $hasValidEntityExpression = $entityExpression.signal.map(_.isRight)
    val $enterRawEntityExpression = new EventBus[String]()
    val $resolvedEntityExpression =
      for {
        raw <- $enterRawEntityExpression.events.delay(500)
        maybeText = raw.stripToOption
        maybeResolved <- Util.sequence(maybeText.map(KBAPI.resolveLabelExpression))
      } yield maybeResolved match {
        case Some(result) => result.left.map(Option(_))
        case None         => Left(None)
      }

    div(
      $resolvedTaxonExpression --> $taxonExpression,
      $resolvedEntityExpression --> $entityExpression,
      h2("OntoTrace"),
      p("Use the OntoTrace query to download a character-by-taxon matrix containing both asserted and inferred presence/absence values for specified kinds of anatomical entities and taxa."),
      div(cls := "alert alert-info", role := "alert",
        p(cls := "bg-info",
          """Matrices generated via the OntoTrace web interface do not include embedded annotations about the asserted published character states entailing each inference.
              To obtain an OntoTrace file with this embedded metadata (often much larger in size), please contact Jim Balhoff at balhoff@renci.org.""")),
      div(
        cls := "panel panel-default",
        div(
          cls := "panel-body",
          form(
            role := "form",
            div(
              cls := "btn-group",
              label(
                cls := "btn btn-default",
                cls.toggle("active") <-- $showSimpleMode,
                onClick.mapTo(SimpleMode) --> $mode,
                "Simple Input"),
              label(
                cls := "btn btn-default",
                cls.toggle("active") <-- $showSimpleMode.map(!_),
                onClick.mapTo(ExpressionMode) --> $mode,
                "OWL Expression Input")),
            div(
              hidden <-- $showSimpleMode.map(!_),
              p(cls := "top-buffer", "Choose a taxonomic group and type of anatomical structure using the autocomplete fields."),
              div(
                h4("Taxon is:"),
                div(
                  cls := "form-group",
                  Views.autocompleteField(
                    KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.VTO)), 20),
                    $simpleTaxonTerm,
                    (term: Term) => term.label,
                    $taxonIRIOpt.writer.contramap[Option[Term]](_.map(_.iri)),
                    Some("any taxonomic group"),
                    Signal.fromValue(false))
                )),
              h5(i("and")),
              div(
                h4("Entity is:"),
                div(
                  cls := "form-group",
                  Views.autocompleteField(
                    KBAPI.ontologyClassSearch(_: String, Some(IRI(Vocab.Uberon)), 20),
                    $simpleEntityTerm,
                    (term: Term) => term.label,
                    $entityIRIOpt.writer.contramap[Option[Term]](_.map(_.iri)),
                    Some("any anatomical entity"),
                    Signal.fromValue(false))
                )),
              div(
                label(
                  input(
                    tpe := "checkbox",
                    controlled(
                      checked <-- $includeParts,
                      onClick.mapToChecked --> $includeParts.writer
                    )
                  ),
                  " Include parts of ",
                  span(
                    hidden <-- $simpleEntityTerm.map(_.nonEmpty),
                    "entity"),
                  span(
                    hidden <-- $simpleEntityTerm.map(_.isEmpty),
                    mark(child.text <-- $simpleEntityTerm.map(_.map(_.label).getOrElse(""))))
                )
              )
            ),
            div(
              hidden <-- $showSimpleMode,
              p(cls := "top-buffer", "Enter taxonomic and anatomical expressions using term labels in OWL Manchester syntax (as in ",
                a(href := "http://protege.stanford.edu", "Protégé"),
                "). Labels containing spaces must be single-quoted, e.g. ", mark("'basihyal bone'"),
                ". Exact label matches will be resolved to term identifiers by the Knowledgebase."),
              p("Example:"),
              div(
                cls := "well",
                dl(
                  dt("Taxon"),
                  dd("Characiformes or Siluriformes"),
                  dt("Entity"),
                  dd("'skeletal element' and ('part of' some fin)"))),
              div(
                h4("Taxon is:"),
                div(
                  cls := "form-group",
                  cls.toggle("has-feedback") <-- $hasTaxonExpressionFeedback,
                  cls.toggle("has-success") <-- $hasValidTaxonExpression,
                  input(
                    title <-- $taxonExpression.signal.map(_.getOrElse("invalid expression")),
                    tpe := "text",
                    placeholder := "taxonomic group expression",
                    cls := "form-control",
                    onInput.mapToValue --> $enterRawTaxonExpression
                  )
                ),
                div(
                  hidden <-- $invalidTaxonError.map(_.isEmpty),
                  cls := "alert alert-danger", role := "alert",
                  child.text <-- $invalidTaxonError.map(_.getOrElse("")))),
              h5(i("and")),
              div(
                h4("Entity is:"),
                div(
                  cls := "form-group",
                  cls.toggle("has-feedback") <-- $hasEntityExpressionFeedback,
                  cls.toggle("has-success") <-- $hasValidEntityExpression,
                  input(
                    title <-- $entityExpression.signal.map(_.getOrElse("invalid expression")),
                    tpe := "text",
                    placeholder := "anatomical entity expression",
                    cls := "form-control",
                    onInput.mapToValue --> $enterRawEntityExpression
                  )
                ),
                div(
                  hidden <-- $invalidEntityError.map(_.isEmpty),
                  cls := "alert alert-danger", role := "alert",
                  child.text <-- $invalidEntityError.map(_.getOrElse("")))
              )
            ),
            div(
              label(
                input(
                  tpe := "checkbox",
                  controlled(
                    checked <-- $includeAllCharacters,
                    onClick.mapToChecked --> $includeAllCharacters
                  )
                ),
                " Include all characters with values (only variable characters are included by default—those with data for both presence and absence)"
              )
            ),
            form(
              a(
                cls := "btn btn-primary btn-block",
                cls.toggle("disabled") <-- $ontotraceDownloadURL.map(_.isEmpty),
                href <-- $ontotraceDownloadURL.map(_.map(_.toString()).getOrElse("")),
                "Download matrix as NeXML"
              )
            )
          )
        )
      )
    )
  }

}
