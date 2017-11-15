package org.phenoscape.kb.ui

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.extras._

object Model {

  // Tweak JSON decoding via Circe for these case classes
  private implicit val config: Configuration = Configuration.default.copy(
    transformKeys = {
      case "iri" => "@id"
      case other => other
    })

  sealed trait ID
  case class IRI(id: String) extends ID
  case class Curie(id: String) extends ID

  @ConfiguredJsonCodec
  case class Term(iri: String, label: String)

  @ConfiguredJsonCodec
  case class Taxon(iri: String, label: String, common_name: Option[String], extinct: Boolean, rank: Option[Term])

  @ConfiguredJsonCodec
  case class Classification(iri: String, label: String, subClassOf: List[Term], equivalentTo: List[Term], superClassOf: List[Term])

}
