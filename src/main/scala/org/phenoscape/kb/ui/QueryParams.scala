package org.phenoscape.kb.ui

import scala.scalajs.js.URIUtils.encodeURIComponent

object QueryParams {

  def toQuery(params: Map[String, Any]): String = (params.map {
    case (key, value) =>
      s"$key=${encodeURIComponent(value.toString)}"
  }).toSeq.sorted.mkString("&")

  def toQueryQ(params: Map[String, Any]): String = {
    val values = toQuery(params)
    if (values.nonEmpty) s"?$values" else ""
  }

  implicit class OptionMap[K, V](val self: Map[K, V]) extends AnyVal {

    def add(optionalItem: Option[(K, V)]): Map[K, V] = optionalItem match {
      case Some(item) => self + item
      case None       => self
    }

  }

}