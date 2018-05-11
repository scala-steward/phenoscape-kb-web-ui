package org.phenoscape.kb.ui

import scala.scalajs.js.URIUtils.encodeURIComponent

import org.phenoscape.kb.ui.Model._

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import outwatch.http.Http
import rxscalajs.Observable
import rxscalajs.dom.Response
import cats.data.Validated
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import rxscalajs.dom.AjaxError

object KBAPI {

  //TODO pass accept header for JSON

  val api: String = "http://kb.phenoscape.org/api"
  //val api: String = "http://localhost:8082"

  def termLabel(iri: IRI): Observable[Term] = get[Term](s"$api/term/label?iri=${enc(iri.id)}")

  def termInfo(iri: IRI): Observable[TermInfo] = get[TermInfo](s"$api/term?iri=${enc(iri.id)}")

  def taxon(iri: IRI): Observable[Taxon] = get[Taxon](s"$api/taxon?iri=${enc(iri.id)}")

  def gene(iri: IRI): Observable[Gene] = get[Gene](s"$api/gene?iri=${enc(iri.id)}")

  def ontologyClassSearch(text: String, definedBy: Option[IRI], limit: Int): Observable[List[Term]] = {
    val params = Map[String, Any](
      "text" -> text,
      "limit" -> limit).add(definedBy.map("definedBy" -> _.id))
    get[ResultList[Term]](s"$api/term/search_classes?${toQuery(params)}").map(_.results)
  }

  def studySearch(text: String): Observable[List[Term]] = {
    val params = Map[String, Any](
      "text" -> text,
      "type" -> Vocab.CharacterStateDataMatrix)
    get[ResultList[Term]](s"$api/term/search?${toQuery(params)}").map(_.results)
  }

  def queryTaxaWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Observable[List[Term]] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "limit" -> limit,
      "offset" -> offset)
      .add(entity.map(e => "entity" -> s"<${e.id}>"))
      .add(quality.map(q => "quality" -> s"<${q.id}>"))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Term]](s"$api/taxon/with_phenotype?${toQuery(params)}").map(_.results)
  }

  def countTaxaWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> s"<${e.id}>"))
      .add(quality.map(q => "quality" -> s"<${q.id}>"))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](s"$api/taxon/with_phenotype?${toQuery(params)}").map(_.total)
  }

  def facetTaxaWithPhenotype(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[List[Facet]] = {
    //FIXME turn 'facet' into enum
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](s"$api/taxon/facet/phenotype/$facet?${toQuery(params)}").map(_.results)
  }

  def queryTaxonAnnotations(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Observable[List[TaxonAnnotation]] = {
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
    get[ResultList[TaxonAnnotation]](s"$api/taxon/annotations?${toQuery(params)}").map(_.results)
  }

  def countTaxonAnnotations(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](s"$api/taxon/annotations?${toQuery(params)}").map(_.total)
  }

  def facetTaxonAnnotations(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[List[Facet]] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](s"$api/taxon/facet/annotations/$facet?${toQuery(params)}").map(_.results)
  }

  def queryTaxonPhenotypes(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Observable[List[Term]] = {
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
    get[ResultList[Term]](s"$api/phenotype/query?${toQuery(params)}").map(_.results)
  }

  def countTaxonPhenotypes(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](s"$api/phenotype/query?${toQuery(params)}").map(_.total)
  }

  def facetTaxonPhenotypes(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[List[Facet]] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](s"$api/phenotype/facet/$facet?${toQuery(params)}").map(_.results)
  }

  def queryStudiesWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean, limit: Int, offset: Int): Observable[List[Term]] = {
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
    get[ResultList[Term]](s"$api/study/query?${toQuery(params)}").map(_.results)
  }

  def countStudiesWithPhenotype(entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[Int] = {
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs,
      "total" -> true)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[Total](s"$api/study/query?${toQuery(params)}").map(_.total)
  }

  def facetStudiesWithPhenotype(facet: String, entity: Option[IRI], quality: Option[IRI], inTaxon: Option[IRI], publication: Option[IRI], parts: Boolean, historicalHomologs: Boolean, serialHomologs: Boolean): Observable[List[Facet]] = {
    //FIXME turn 'facet' into enum
    val params = Map[String, Any](
      "parts" -> parts,
      "historical_homologs" -> historicalHomologs,
      "serial_homologs" -> serialHomologs)
      .add(entity.map(e => "entity" -> e.id))
      .add(quality.map(q => "quality" -> q.id))
      .add(inTaxon.map("in_taxon" -> _.id))
      .add(publication.map("publication" -> _.id))
    get[ResultList[Facet]](s"$api/study/facet/$facet?${toQuery(params)}").map(_.results)
  }

  def similarityMatches(subject: IRI, corpusGraph: IRI, limit: Int, offset: Int): Observable[ResultList[SimilarityMatch]] = {
    val params = Map(
      "iri" -> subject.id,
      "corpus_graph" -> corpusGraph.id,
      "limit" -> limit,
      "offset" -> offset)
    get[ResultList[SimilarityMatch]](s"$api/similarity/query?${toQuery(params)}")
  }

  def similarityProfileSize(iri: IRI): Observable[Int] = get[Total](s"$api/similarity/profile_size?iri=${enc(iri.id)}").map(_.total)

  def bestMatches(queryProfile: IRI, queryGraph: IRI, corpusProfile: IRI, corpusGraph: IRI): Observable[ResultList[SimilarityAnnotationMatch]] = {
    val params = Map(
      "query_iri" -> queryProfile.id,
      "query_graph" -> queryGraph.id,
      "corpus_iri" -> corpusProfile.id,
      "corpus_graph" -> corpusGraph.id)
    get[ResultList[SimilarityAnnotationMatch]](s"$api/similarity/best_matches?${toQuery(params)}")
  }

  def similarityCorpusSize(corpusGraph: IRI): Observable[Int] = get[Total](s"$api/similarity/corpus_size?corpus_graph=${enc(corpusGraph.id)}").map(_.total)

  def classification(iri: IRI, definedBy: IRI): Observable[Classification] = get[Classification](s"$api/term/classification?iri=${enc(iri.id)}&definedBy=${enc(definedBy.id)}")

  def taxonCommonGroup(taxon: IRI): Observable[TaxonGroup] = get[TaxonGroup](s"$api/taxon/group?iri=${enc(taxon.id)}")

  def kbInfo: Observable[KBInfo] = get[KBInfo](s"$api/kb/annotation_summary")

  def resolveLabelExpression(expression: String): Observable[Validated[String, String]] =
    Http.get(Observable.of(s"$api/term/resolve_label_expression?expression=${enc(expression)}")).catchError { e =>
      // working around bug in error handling by Outwatch Http
      val er = e.asInstanceOf[AjaxError]
      Observable.just(Response(er.xhr.responseText, er.status.toInt, er.xhr.responseType, er.xhr, null))
    }.map { res =>
      res.status match {
        case 200 => Valid(res.body)
        case _   => Invalid(res.body)
      }
    }

  def ontotraceURL(taxonExpression: String, entityExpression: String, includeParts: Boolean, variableOnly: Boolean): String =
    s"$api/ontotrace?taxon=${enc(taxonExpression)}&entity=${enc(entityExpression)}&variable_only=$variableOnly&parts=$includeParts"

  private def enc(value: String): String = encodeURIComponent(value)

  private def get[T](uri: String)(implicit evidence: Decoder[T]): Observable[T] = toCaseClass[T](Http.get(Observable.of(uri)))

  private def toCaseClass[T](response: Observable[Response])(implicit evidence: Decoder[T]): Observable[T] = response.map(res => {
    val decoded = decode[T](res.body)
    decoded match {
      case Left(error) =>
        println(error)
        println(s"Failed decoding JSON response: ${res.body.take(100)}...")
      case _ => ()
    }
    decoded
  }).collect { case Right(value) => value }

  private def toQuery(params: Map[String, Any]): String = (params.map {
    case (key, value) =>
      s"$key=${enc(value.toString)}"
  }).toSeq.sorted.mkString("&")

  private final case class Total(total: Int)

  private implicit class OptionMap[K, V](val self: Map[K, V]) extends AnyVal {

    def add(optionalItem: Option[(K, V)]): Map[K, V] = optionalItem match {
      case Some(item) => self + item
      case None       => self
    }

  }

}