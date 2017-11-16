package org.phenoscape.kb.ui

import java.util.regex.Matcher

import Model.Curie
import Model.IRI

object Vocab {

  val Genus = "http://purl.obolibrary.org/obo/TAXRANK_0000005"
  val Species = "http://purl.obolibrary.org/obo/TAXRANK_0000006"
  val GenusOrSpecies = Set(Genus, Species)
  val VTO = "http://purl.obolibrary.org/obo/vto.owl"
  val Uberon = "http://purl.obolibrary.org/obo/uberon.owl"
  val TaxonSimilarityCorpus = "http://kb.phenoscape.org/sim/taxa"
  val GeneSimilarityCorpus = "http://kb.phenoscape.org/sim/genes"

  private val Prefixes = Map(
    "VTO" -> "http://purl.obolibrary.org/obo/VTO_",
    "TAXRANK" -> "http://purl.obolibrary.org/obo/TAXRANK_",
    "UBERON" -> "http://purl.obolibrary.org/obo/UBERON_",
    "ZFIN" -> "http://zfin.org/")

  private val Expansions = Prefixes.map(_.swap)

  def expand(curie: Curie): IRI = {
    val items = curie.id.split(":", 2)
    if ((items.size > 1) && Prefixes.contains(items(0))) IRI(s"${Prefixes(items(0))}${items(1)}")
    else IRI(curie.id)
  }

  def compact(iri: IRI): Curie = Curie(
    Expansions.keys.find(iri.id.startsWith)
      .map(expansion => iri.id.replaceFirst(
        Matcher.quoteReplacement(expansion),
        s"${Expansions(expansion)}:"))
      .getOrElse(iri.id))

}