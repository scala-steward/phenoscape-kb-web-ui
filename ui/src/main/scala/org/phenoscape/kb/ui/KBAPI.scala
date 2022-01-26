package org.phenoscape.kb.ui

import com.raquo.airstream.core.EventStream
import io.circe.Decoder
import io.circe.generic.auto._
import org.phenoscape.kb.ui.Model._
import sttp.client3._
import sttp.client3.circe._
import sttp.model.Uri

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

object KBAPI {

  @js.native
  @JSGlobalScope
  object Conf extends js.Object {
    var KB_ENDPOINT: String = js.native
  }

  val api: String = Conf.KB_ENDPOINT

  private val backend = FetchBackend()

  private final case class Total(total: Int)

  def termLabel(iri: IRI): EventStream[Term] = get[Term](uri"$api/term/label?iri=${iri.id}")

  def termInfo(iri: IRI): EventStream[TermInfo] = get[TermInfo](uri"$api/term?iri=${iri.id}")

  def taxon(iri: IRI): EventStream[Taxon] = get[Taxon](uri"$api/taxon?iri=${iri.id}")

  def gene(iri: IRI): EventStream[Gene] = get[Gene](uri"$api/gene?iri=${iri.id}")

  def ontologyClassSearch(text: String, definedBy: Option[IRI], limit: Int): EventStream[List[Term]] =
    get[ResultList[Term]](uri"$api/term/search_classes?text=$text&limit=$limit&definedBy=${definedBy.map(_.id)}").map(_.results)

  def geneSearch(text: String, limit: Int): EventStream[List[Gene]] =
    get[ResultList[Gene]](uri"$api/gene/search?text=$text&limit=$limit").map(_.results)

  def studySearch(text: String): EventStream[List[Term]] =
    get[ResultList[Term]](uri"$api/term/search?text=$text&type=${Vocab.CharacterStateDataMatrix}").map(_.results)

  def studyInfo(iri: IRI): EventStream[Study] = get[Study](uri"$api/study?iri=${iri.id}")

  def queryTaxaWithPhenotypeURL(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Uri = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "limit" -> limit,
      "offset" -> offset)
      .add(entity.map(e => "entity" -> <>(e)))
      .add(quality.map(q => "quality" -> <>(q)))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    uri"$api/taxon/with_phenotype?$params"
  }

  def queryTaxaWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): EventStream[List[Term]] = {
    val url = queryTaxaWithPhenotypeURL(entity, quality, inTaxon, publication, parts, historicalHomologs, serialHomologs, limit, offset)
    get[ResultList[Term]](url).map(_.results)
  }

  def countTaxaWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> s"<${e.id}>"))
      .add(quality.map(q => "quality" -> s"<${q.id}>"))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](uri"$api/taxon/with_phenotype?$params").map(_.total)
  }

  def facetTaxaWithPhenotype(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[List[Facet]] = {
    //FIXME turn 'facet' into enum
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](uri"$api/taxon/facet/phenotype/$facet?$params").map(_.results)
  }

  def queryGenesWithPhenotypeURL(entity: Option[IRI], quality: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Uri = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "limit" -> limit,
      "offset" -> offset)
      .add(entity.map("iri" -> _.id))
      .add(quality.map("quality" -> _.id))
    uri"$api/gene/affecting_entity_phenotype?$params"
  }

  def queryGenesWithPhenotype(entity: Option[IRI], quality: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): EventStream[List[Term]] = {
    val url = queryGenesWithPhenotypeURL(entity, quality, parts, historicalHomologs, serialHomologs, limit, offset)
    get[ResultList[Term]](url).map(_.results)
  }

  def countGenesWithPhenotype(entity: Option[IRI], quality: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map("iri" -> _.id))
      .add(quality.map("quality" -> _.id))
    get[Total](uri"$api/gene/affecting_entity_phenotype?$params").map(_.total)
  }

  def facetGenesWithPhenotype(facet: String, entity: Option[IRI], quality: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[List[Facet]] = {
    //FIXME turn 'facet' into enum
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
    get[ResultList[Facet]](uri"$api/gene/facet/phenotype/$facet?$params").map(_.results)
  }

  def queryTaxonAnnotationsURL(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Uri = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "limit" -> limit,
      "offset" -> offset)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    uri"$api/taxon/annotations?$params"
  }

  def queryTaxonAnnotations(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): EventStream[List[TaxonAnnotation]] = {
    val url = queryTaxonAnnotationsURL(entity, quality, inTaxon, publication, parts, historicalHomologs, serialHomologs, limit, offset)
    get[ResultList[TaxonAnnotation]](url).map(_.results)
  }

  def countTaxonAnnotations(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](uri"$api/taxon/annotations?$params").map(_.total)
  }

  def facetTaxonAnnotations(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[List[Facet]] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](uri"$api/taxon/facet/annotations/$facet?$params").map(_.results)
  }

  def queryTaxonPhenotypesURL(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Uri = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "limit" -> limit,
      "offset" -> offset)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    uri"$api/phenotype/query?$params"
  }

  def queryTaxonPhenotypes(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): EventStream[List[Term]] = {
    val url = queryTaxonPhenotypesURL(entity, quality, inTaxon, publication, parts, historicalHomologs, serialHomologs, limit, offset)
    get[ResultList[Term]](url).map(_.results)
  }

  def countTaxonPhenotypes(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](uri"$api/phenotype/query?$params").map(_.total)
  }

  def facetTaxonPhenotypes(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[List[Facet]] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](uri"$api/phenotype/facet/$facet?$params").map(_.results)
  }

  def queryStudiesWithPhenotypeURL(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Uri = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "limit" -> limit,
      "offset" -> offset)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    uri"$api/study/query?$params"
  }

  def queryStudiesWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): EventStream[List[Term]] = {
    val url = queryStudiesWithPhenotypeURL(entity, quality, inTaxon, publication, parts, historicalHomologs, serialHomologs, limit, offset)
    get[ResultList[Term]](url).map(_.results)
  }

  def countStudiesWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](uri"$api/study/query?$params").map(_.total)
  }

  def facetStudiesWithPhenotype(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): EventStream[List[Facet]] = {
    //FIXME turn 'facet' into enum
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](uri"$api/study/facet/$facet?$params").map(_.results)
  }

  def annotationSources(taxon: IRI, phenotype: IRI): EventStream[List[AnnotationSource]] =
    get[ResultList[AnnotationSource]](uri"$api/taxon/annotation/sources?taxon=${taxon.id}&phenotype=${phenotype.id}").map(_.results)

  def annotationSources(annotation: TaxonAnnotation): EventStream[List[AnnotationSource]] = annotationSources(annotation.taxon.iri, annotation.phenotype.iri)

  def similarityMatches(subject: IRI, corpusGraph: IRI, limit: Int, offset: Int): EventStream[ResultList[SimilarityMatch]] = {
    val params = Map(
      "iri" -> subject.id,
      "corpus_graph" -> corpusGraph.id,
      "limit" -> limit,
      "offset" -> offset)
    get[ResultList[SimilarityMatch]](uri"$api/similarity/query?$params")
  }

  def similarityProfileSize(iri: IRI): EventStream[Int] = get[Total](uri"$api/similarity/profile_size?iri=${iri.id}").map(_.total)

  def bestMatches(queryProfile: IRI, queryGraph: IRI, corpusProfile: IRI, corpusGraph: IRI): EventStream[ResultList[SimilarityAnnotationMatch]] = {
    val params = Map(
      "query_iri" -> queryProfile.id,
      "query_graph" -> queryGraph.id,
      "corpus_iri" -> corpusProfile.id,
      "corpus_graph" -> corpusGraph.id)
    get[ResultList[SimilarityAnnotationMatch]](uri"$api/similarity/best_matches?$params")
  }

  def similarityCorpusSize(corpusGraph: IRI): EventStream[Int] = get[Total](uri"$api/similarity/corpus_size?corpus_graph=${corpusGraph.id}").map(_.total)

  def classification(iri: IRI, definedBy: IRI): EventStream[Classification] = get[Classification](uri"$api/term/classification?iri=${iri.id}&definedBy=${definedBy.id}")

  def taxonCommonGroup(taxon: IRI): EventStream[TaxonGroup] = get[TaxonGroup](uri"$api/taxon/group?iri=${taxon.id}")

  def kbInfo: EventStream[KBInfo] = get[KBInfo](uri"$api/kb/annotation_summary")

  def homologyAnnotations(entity: IRI): EventStream[List[HomologyAnnotation]] = get[ResultList[HomologyAnnotation]](uri"$api/entity/homology?entity=${entity.id}").map(_.results)

  def resolveLabelExpression(expression: String): EventStream[Either[String, String]] =
    EventStream.fromFuture(basicRequest.get(uri"$api/term/resolve_label_expression?expression=$expression")
      .response(asString).send(backend)
      .map(_.body))

  def ontotraceURL(taxonExpression: String, entityExpression: String, includeParts: Boolean, variableOnly: Boolean): Uri =
    uri"$api/ontotrace?taxon=$taxonExpression&entity=$entityExpression&variable_only=$variableOnly&parts=$includeParts"

  private def get[T](uri: Uri)(implicit evidence: Decoder[T]): EventStream[T] = {
    val future = basicRequest
      .header("Accept", "application/json")
      .get(uri).response(asJson[T])
      .readTimeout(5.minutes)
      .send(backend).map { response =>
      response.body match {
        case Right(_)    => ()
        case Left(error) => println(error.getMessage)
          println(s"Failed decoding JSON response: ${response.body.toString.take(100)}...")
      }
      response.body
    }.collect { case Right(value) => value }
    EventStream.fromFuture(future, true)
  }

  private def <>(iri: IRI): String = s"<${iri.id}>"

  implicit class OptionMap[K, V](val self: Map[K, V]) extends AnyVal {

    def add(optionalItem: Option[(K, V)]): Map[K, V] = optionalItem match {
      case Some(item) => self + item
      case None       => self
    }

  }

}
