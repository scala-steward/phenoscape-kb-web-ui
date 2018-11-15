package org.phenoscape.kb.ui

import io.circe._
import io.circe.generic.auto._
import scala.scalajs.js.Date

object Model {

  // We could use circe-generic-extras and @ConfiguredJsonCodec to avoid implementing JSON decoder objects,
  // but it requires the macro compiler plug-in and makes Scala-IDE unhappy. This is simpler.

  sealed trait ID

  final case class IRI(id: String) extends ID

  object IRI {

    implicit val decoder: Decoder[IRI] = Decoder.decodeString.map(IRI.apply)

  }

  final case class Curie(id: String) extends ID

  final case class Term(iri: IRI, label: String)

  object Term {

    implicit val decoder: Decoder[Term] = Decoder.forProduct2("@id", "label")(Term.apply)

  }

  final case class TermInfo(term: Term, definition: Option[String], synonyms: List[(IRI, String)], relationships: List[Relation])

  object TermInfo {

    implicit val decoder: Decoder[TermInfo] = Decoder.forProduct5("@id", "label", "definition", "synonyms", "relationships") { (id: String, label: String, definition: Option[String], synonyms: List[Map[String, String]], relationships: List[Relation]) =>
      TermInfo(Term(IRI(id), label), definition.filterNot(_.isEmpty), synonyms.map(syn => IRI(syn("property")) -> syn("value")), relationships)
    }
  }

  final case class Relation(property: Term, value: Term)

  final case class Taxon(iri: IRI, label: String, commonName: Option[String], extinct: Boolean, rank: Option[Term])

  object Taxon {

    implicit val decoder: Decoder[Taxon] = Decoder.forProduct5("@id", "label", "common_name", "extinct", "rank")(Taxon.apply)

  }

  final case class Study(iri: IRI, label: String, citation: String)

  object Study {

    implicit val decoder: Decoder[Study] = Decoder.forProduct3("@id", "label", "citation")(Study.apply)

  }

  final case class Gene(iri: IRI, label: String, taxon: Term)

  object Gene {

    implicit val decoder: Decoder[Gene] = Decoder.forProduct3("@id", "label", "taxon")(Gene.apply)

  }

  final case class TaxonAnnotation(taxon: Term, phenotype: Term)

  object TaxonAnnotation {

    implicit val decoder: Decoder[TaxonAnnotation] = Decoder.forProduct2("taxon", "phenotype")(TaxonAnnotation.apply)

  }

  final case class AnnotationSource(study: Term, characterNum: Int, characterText: String, stateText: String)

  object AnnotationSource {

    implicit val decoder: Decoder[AnnotationSource] = Decoder.forProduct4("publication", "character_num", "character_text", "state_text")(AnnotationSource.apply)

  }

  final case class Classification(iri: IRI, label: String, subClassOf: List[Term], equivalentTo: List[Term], superClassOf: List[Term])

  object Classification {

    implicit val decoder: Decoder[Classification] = Decoder.forProduct5("@id", "label", "subClassOf", "equivalentTo", "superClassOf")(Classification.apply)

  }

  final case class ResultList[T](results: List[T])

  /**
    * Match between two profiles.
    */
  final case class SimilarityMatch(matchProfile: Term, medianScore: Double, expectScore: Double)

  object SimilarityMatch {

    implicit val decoder: Decoder[SimilarityMatch] = Decoder.forProduct3("match_profile", "median_score", "expect_score")(SimilarityMatch.apply)

  }

  final case class SimilaritySubsumerTerm(term: Term, ic: Double, disparity: Double)

  final case class SimilarityAnnotationMatch(queryAnnotation: IRI, corpusAnnotation: IRI, bestSubsumer: SimilaritySubsumerTerm)

  object SimilarityAnnotationMatch {

    implicit val iriDecoder: Decoder[IRI] = Decoder.forProduct1("@id")(IRI.apply)

    implicit val decoder: Decoder[SimilarityAnnotationMatch] = Decoder.forProduct3("query_annotation", "corpus_annotation", "best_subsumer")(SimilarityAnnotationMatch.apply)

  }

  final case class TaxonGroup(label: String, phylopic: IRI)

  final case class Facet(term: Term, count: Int)

  final case class KBInfo(buildDate: Date, annotatedMatrices: Int, annotatedTaxa: Int, annotatedCharacters: Int, annotatedStates: Int)

  object KBInfo {

    private def toDate(text: String): Date = new Date(Date.parse(text))

    implicit val decoder: Decoder[KBInfo] = Decoder.forProduct5("build_time", "annotated_matrices", "annotated_taxa", "annotated_characters", "annotated_states") { (date: String, matrices: Int, taxa: Int, characters: Int, states: Int) =>
      KBInfo(toDate(date), matrices, taxa, characters, states)
    }

  }

  final case class HomologyAnnotation(source: String, subject: IRI, relation: IRI, `object`: IRI, subjectTaxon: IRI, objectTaxon: IRI, evidence: IRI, negated: Boolean)

}
