package org.phenoscape.kb.ui

import org.phenoscape.kb.ui.Model.IRI

import outwatch.Sink
import outwatch.dom.Handler
import rxscalajs.Observable

object Util {

  /**
    * A handler for toggling CSS classes in response to events
    */
  def createCSSClassHandler(initialClasses: String*): Handler[(String, Boolean)] = Sink.createHandler[(String, Boolean)](initialClasses.map(cls => cls -> true): _*)

  /**
    * Map CSS class toggles to a usable attribute value
    */
  def observableCSS(toggles: Observable[(String, Boolean)]): Observable[String] = toggles.scan(Set.empty[String]) { (classes, action) =>
    action match {
      case (cls, true)  => classes + cls
      case (cls, false) => classes - cls
    }
  }.map(_.mkString(" "))

  def sequence[T](optObs: Option[Observable[T]]): Observable[Option[T]] = optObs match {
    case Some(obs) => obs.map(Option(_))
    case None      => Observable.of(None)
  }

  def taxonThumbnailIRI(phylopic: IRI): IRI = {
    val uuid = phylopic.id.replaceAllLiterally("http://phylopic.org/image/", "").replaceAllLiterally("/", "")
    IRI(s"http://phylopic.org/assets/images/submissions/$uuid.64.png")
  }

  //TODO KB services should provide model organism type in gene info
  def modelOrganismThumbnailURL(geneIRI: IRI): String = {
    val id = geneIRI.id
    if (id.startsWith("http://www.informatics.jax.org")) "http://phylopic.org/assets/images/submissions/6b2b98f6-f879-445f-9ac2-2c2563157025.64.png"
    else if (id.startsWith("http://zfin.org/")) "http://phylopic.org/assets/images/submissions/199829d3-183a-4eb3-a35a-a8705d28cb56.64.png"
    else if (id.startsWith("http://www.xenbase.org/")) "http://phylopic.org/assets/images/submissions/cd0f49a1-4adf-448e-859c-b703a73b9481.64.png"
    else if (id.startsWith("http://www.ncbi.nlm.nih.gov")) "http://phylopic.org/assets/images/submissions/a9f4ebd5-53d7-4de9-9e66-85ff6c2d513e.64.png"
    else ""
  }

  def interpolate[T](elem: T, list: List[T]): List[T] = list match {
    case Nil             => Nil
    case last @ _ :: Nil => last
    case x :: xs         => x :: elem :: interpolate(elem, xs)
  }

  implicit class StringOps(val self: String) extends AnyVal {

    def stripToOption: Option[String] = if (self.isEmpty) None else Some(self)

  }

  def linkToTaxon(iri: IRI): String = s"#/taxon/${Vocab.compact(iri).id}"

  def linkToEntity(iri: IRI): String = s"#/entity/${Vocab.compact(iri).id}"

  def linkToGene(iri: IRI): String = s"#/gene/${Vocab.compact(iri).id}"

}