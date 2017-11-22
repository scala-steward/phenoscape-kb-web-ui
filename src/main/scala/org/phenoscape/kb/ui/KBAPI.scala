package org.phenoscape.kb.ui

import scala.scalajs.js.URIUtils.encodeURIComponent

import org.phenoscape.kb.ui.Model._

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import outwatch.http.Http
import rxscalajs.Observable
import rxscalajs.dom.Response

object KBAPI {

  //TODO pass accept header for JSON

  val api: String = "http://kb.phenoscape.org/api"

  def termLabel(iri: IRI): Observable[Term] = get[Term](s"$api/term/label?iri=${enc(iri.id)}")

  def taxon(iri: IRI): Observable[Taxon] = get[Taxon](s"$api/taxon?iri=${enc(iri.id)}")

  def gene(iri: IRI): Observable[Gene] = get[Gene](s"$api/gene?iri=${enc(iri.id)}")

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

}