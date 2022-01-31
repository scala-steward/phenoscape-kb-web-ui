package org.phenoscape.kb.ui

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.Page

object AboutPageView {

  def view(updates: WriteBus[Page]): HtmlElement = {
    val $data = KBAPI.kbInfo
    val dateString = $data.map(_.buildDate.toLocaleDateString())
    div(
      h2("About the Knowledgebase"),
      p("Data and ontologies in the KB were last updated on ", strong(child.text <-- dateString), "."),
      div(
        h3("Data"),
        h4("Phenoscape-annotated evolutionary data"),
        p(a(href := "https://github.com/phenoscape/phenoscape-data", "Available at GitHub"), " in NeXML format."),
        p("A ", a(href := "http://kb.phenoscape.org/api/kb/annotation_report", "character annotation report"), " is available for all the NeXML data loaded into the KB."),
        h4("Model organism gene phenotypes and expression locations"),
        p("Model organism data are imported via RDF datasets provided by the ", a(href := "https://monarchinitiative.org", "Monarch Initiative"), "."),
        ul(
          li(a(href := "http://zfin.org", "ZFIN"), " (zebrafish)"),
          li(a(href := "http://xenbase.org", "Xenbase"), " (frog)"),
          li(a(href := "http://www.informatics.jax.org", "MGI"), " (mouse)"),
          li(a(href := "http://www.human-phenotype-ontology.org/", "Human phenotype ontology project")))),
      div(
        h3("Ontologies"),
        p("Logical relationships expressed in ontologies tie together everything in the Knowledgebase and drive functionality. The KB includes and builds on these community-developed ontologies:"),
        ul(
          li(a(href := "http://uberon.org", "Uberon ontology of metazoan anatomy")),
          li(a(href := "http://wiki.obofoundry.org/ontology/pato.html", "Phenotype and trait ontology (PATO)")),
          li(a(href := "http://wiki.obofoundry.org/ontology/bspo.html", "Biospatial ontology (BSPO)")),
          li(a(href := "http://zfin.org/action/ontology/search", "Zebrafish anatomy (ZFA)")),
          li(a(href := "http://www.xenbase.org/anatomy/xao.do?method=display", "Xenopus anatomy (XAO)")),
          li(a(href := "http://emouseatlas.org/emap/home.html", "Mouse developmental anatomy (EMAPA)")),
          li(a(href := "http://www.informatics.jax.org/searches/MP_form.shtml", "Mammalian phenotype ontology (MP)")),
          li(a(href := "http://www.human-phenotype-ontology.org/", "Human phenotype ontology (HP)")),
          li(a(href := "https://github.com/oborel/obo-relations", "OBO relations ontology (RO)")),
          li(a(href := "http://www.geneontology.org/", "Gene ontology (GO)")))),
      div(
        h3("Software"),
        p("All the software developed by the Phenoscape project is open source and ", a(href := "https://github.com/phenoscape", "available at GitHub"), ".")),
      div(
        h3("Acknowledgments"),
        p("Organism silhouettes are courtesy of ", a(href := "http://phylopic.org/", "PhyloPic"), ". In particular, we thank artists ", a(href := "http://phylopic.org/image/6b2b98f6-f879-445f-9ac2-2c2563157025", "Madeleine Price Ball"), " and ", a(href := "http://phylopic.org/image/cd0f49a1-4adf-448e-859c-b703a73b9481/", "Sarah Werning"), ".")))
  }

}
