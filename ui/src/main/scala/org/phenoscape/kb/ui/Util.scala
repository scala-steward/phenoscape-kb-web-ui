package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveElement
import org.phenoscape.kb.ui.Model.IRI

object Util {

  def interpolate[T](elem: T, list: List[T]): List[T] = list match {
    case Nil             => Nil
    case last @ _ :: Nil => last
    case x :: xs         => x :: elem :: interpolate(elem, xs)
  }

  implicit class StringOps(val self: String) extends AnyVal {

    def stripToOption: Option[String] = if (self.isEmpty) None else Some(self)

  }

  def sequence[T](optObs: Option[EventStream[T]]): EventStream[Option[T]] = optObs match {
    case Some(obs) => obs.map(Option(_))
    case None      => EventStream.fromValue(None)
  }

  def makeAction[T, S](current: Signal[S], sink: Observer[S])(fn: (T, S) => S): (Observer[T], Binder[ReactiveElement.Base]) = {
    val (actionStream, action) = EventStream.withObserver[T]
    val binder = actionStream.withCurrentValueOf(current).map { v =>
      fn(v._1, v._2)
    } --> sink
    (action, binder)
  }

  def taxonThumbnailIRI(phylopic: IRI): IRI = {
    val uuid = phylopic.id.replace("http://phylopic.org/image/", "").replace("/", "")
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

}
