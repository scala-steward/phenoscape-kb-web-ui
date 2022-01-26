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
  val PATO = "http://purl.obolibrary.org/obo/pato.owl"
  val TaxonSimilarityCorpus = "http://kb.phenoscape.org/sim/taxa"
  val GeneSimilarityCorpus = "http://kb.phenoscape.org/sim/genes"
  val CharacterStateDataMatrix = "http://purl.obolibrary.org/obo/CDAO_0000056"
  val RDFSSubClassOf = "http://www.w3.org/2000/01/rdf-schema#"

  private val Prefixes = Map(
    "VTO" -> "http://purl.obolibrary.org/obo/VTO_",
    "TAXRANK" -> "http://purl.obolibrary.org/obo/TAXRANK_",
    "UBERON" -> "http://purl.obolibrary.org/obo/UBERON_",
    "CL" -> "http://purl.obolibrary.org/obo/CL_",
    "ZFA" -> "http://purl.obolibrary.org/obo/ZFA_",
    "PATO" -> "http://purl.obolibrary.org/obo/PATO_",
    "ZFIN" -> "http://zfin.org/",
    "MGI" -> "http://www.informatics.jax.org/marker/MGI:",
    "ncbigene" -> "http://www.ncbi.nlm.nih.gov/gene/",
    "DOI" -> "http://dx.doi.org/",
    "HDL" -> "http://hdl.handle.net/",
    "JSTOR" -> "http://www.jstor.org/stable/",
    "ps" -> "http://purl.org/phenoscape/vocab.owl#")

  val synonymTypes: Map[IRI, String] = Map(
    IRI("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym") -> "exact",
    IRI("http://www.geneontology.org/formats/oboInOwl#hasNarrowSynonym") -> "narrow",
    IRI("http://www.geneontology.org/formats/oboInOwl#hasBroadSynonym") -> "broad",
    IRI("http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym") -> "related")

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
