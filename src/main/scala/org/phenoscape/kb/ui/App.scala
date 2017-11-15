package org.phenoscape.kb.ui

import scala.scalajs.js.JSApp

import org.phenoscape.kb.ui.Model.IRI

import outwatch.dom._
import outwatch.router.BaseUrl
import outwatch.router.Router
import rxscalajs.Observable
import Model.Curie

object App extends JSApp {

  def main(): Unit = {
    OutWatch.render("#app", KBRouter())
  }

  object KBRouter extends Router {

    sealed trait Page
    object HomePage extends Page
    case class TaxonURL(id: String) extends Page

    val baseUrl: BaseUrl = BaseUrl.until_# + "#"

    override def baseLayout(node: Observable[VNode]): VNode = BaseLayout.view(node)

    val config = RouterConfig { builder =>
      import builder._

      builder.rules(
        "/home".const(HomePage) ~> Home(),
        ("/taxon" / string(".+")).caseClass[TaxonURL] ~> { case TaxonURL(id) => TaxonPage(TaxonPage.State(Vocab.expand(Curie(id)))) })
        .notFound(Redirect(HomePage, replace = true))
    }
  }

}