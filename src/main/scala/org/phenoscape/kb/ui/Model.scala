package org.phenoscape.kb.ui

import io.circe._

object Model {

  // We could use circe-generic-extras and @ConfiguredJsonCodec to avoid implementing JSON decoder objects,
  // but it requires the macro compiler plug-in and makes Scala-IDE unhappy. This is simpler.

  sealed trait ID
  case class IRI(id: String) extends ID
  case class Curie(id: String) extends ID

  case class Term(iri: String, label: String)

  object Term {

    implicit val decoder: Decoder[Term] = Decoder.forProduct2("@id", "label")(Term.apply)

  }

  case class Taxon(iri: String, label: String, common_name: Option[String], extinct: Boolean, rank: Option[Term])

  object Taxon {

    implicit val decoder: Decoder[Taxon] = Decoder.forProduct5("@id", "label", "common_name", "extinct", "rank")(Taxon.apply)

  }

  case class Classification(iri: String, label: String, subClassOf: List[Term], equivalentTo: List[Term], superClassOf: List[Term])

  object Classification {

    implicit val decoder: Decoder[Classification] = Decoder.forProduct5("@id", "label", "subClassOf", "equivalentTo", "superClassOf")(Classification.apply)

  }

}
