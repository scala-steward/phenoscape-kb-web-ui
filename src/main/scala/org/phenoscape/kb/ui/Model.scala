package org.phenoscape.kb.ui

import io.circe._
import io.circe.generic.auto._

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

  final case class Taxon(iri: IRI, label: String, commonName: Option[String], extinct: Boolean, rank: Option[Term])

  object Taxon {

    implicit val decoder: Decoder[Taxon] = Decoder.forProduct5("@id", "label", "common_name", "extinct", "rank")(Taxon.apply)

  }

  final case class Gene(iri: IRI, label: String, taxon: Term)

  object Gene {

    implicit val decoder: Decoder[Gene] = Decoder.forProduct3("@id", "label", "taxon")(Gene.apply)

  }

  final case class TaxonAnnotation(taxon: Term, phenotype: Term, source: Term)

  object TaxonAnnotation {

    implicit val decoder: Decoder[TaxonAnnotation] = Decoder.forProduct3("taxon", "phenotype", "study")(TaxonAnnotation.apply)

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

}
