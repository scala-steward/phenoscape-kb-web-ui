package org.phenoscape.kb.ui

import outwatch.dom._
import outwatch.redux.Component

object Home extends Component {

  //object Action

  class State extends ComponentState {

    def evolve = {
      case _ => this
    }

  }

  def apply(): VNode = {
    div(
      cls := "row",
      div(
        cls := "col-sm-9",
        div(
          cls := "alert alert-info",
          role := "alert",
          p("Welcome to the ", a(href := "#/about/phenoscape/kb", cls := "alert-link", "Phenoscape Knowledgebase"),
            """ (or KB), a datastore of computable phenotypes for studies of evolution and genetics. 
              You can explore a large number of manually curated annotations in the KB derived from the literature. 
              These annotations, taken from trusted literature sources, report that a taxon (or genotype) shows variation 
              in some quality (e.g. shape, size, etc.) in some entity (anatomical part), using controlled vocabulary terms 
              from community ontologies that allow for """, i("semantic reasoning"),
            " across diverse phenotypes from different organisms."),
          p(
            """Much of the recent evolutionary data in the KB is from the appendicular skeleton of sarcopterygian vertebrates, 
            the clade in which the “fin to limb” transition occurred. Data from the first iteration of the KB, with broad 
            coverage of ostariophysan fishes, is included here as well, and also available via the """,
            a(href := "http://fish.phenoscape.org/", cls := "alert-link", "previous KB software"),
            ". Phenotype data from gene disruptions have been imported for mouse (from ",
            a(href := "http://www.informatics.jax.org", cls := "alert-link", "MGI"),
            "), human (", a(href := "http://www.human-phenotype-ontology.org", cls := "alert-link", "HPO"), "), zebrafish (",
            a(href := "http://zfin.org/", cls := "alert-link", "ZFIN"), "), and frog (",
            a(href := "http://www.xenbase.org/", cls := "alert-link", "Xenbase"), ")."),
          p(
            "New features available in the KB include:",
            ul(
              li(
                "Generation of synthetic presence/absence comparative data matrices (",
                a(href := "http://dx.doi.org/10.1093/sysbio/syv031", cls := "alert-link", "Dececchi ", i("et al"), "., 2015"), ")"),
              li(
                "Semantic similarity search for evolutionary variation related to gene phenotypes (",
                a(href := "http://dx.doi.org/10.1002/dvg.22878", cls := "alert-link", "Manda ", i("et al"), "., 2015"), ", ",
                a(href := "http://phenoday2015.bio-lark.org/pdf/9.pdf", cls := "alert-link", "Balhoff ", i("et al"), "., 2015"), ")")))),
        div(
          cls := "row",
          examplePanel(
            "Search for taxa, anatomical structures, or genes",
            p("Go straight to an item of interest using the search box at the top right of every page."),
            p(i("Try entering ", b("Cypriniformes"), ", ", b("femur"), ", or ", b("bmp1a"), "."))),
          examplePanel(
            "Find phenotypes annotated to a taxon",
            p("View published phenotypes for a given taxon, aggregated from multiple comparative studies."),
            p(i(
              "See ",
              a(
                href := "#/taxon/VTO:0036225",
                "phenotypes for the channel catfish ", b("Ictalurus punctatus")),
              "."))),
          examplePanel(
            "Find phenotypic data for an anatomical structure",
            p("An anatomical ontology puts phenotypic data into a rich anatomical and developmental context."),
            p(i(
              "Find ",
              a(href := "#/entity/http://purl.obolibrary.org/obo/UBERON_0002513?tab=taxa&taxatab=phenotypes", "taxa with phenotypic data"),
              " for any type of ", b("endochondral bone"), ". Or ",
              a(href := "#/entity/http://purl.obolibrary.org/obo/UBERON_0002513?tab=genes&taxatab=phenotypes&genestab=phenotypes", "genes from mouse, zebrafish, frog, or human with phenotypic effects"),
              "."))),
          examplePanel(
            "Find evolutionary variation similar to genetic phenotypes",
            p("Using semantic similarity, the KB can perform fuzzy matching between suites of phenotypes, such as the various phenotypic effects of a gene vs. the profile of phenotypic variation within a given taxonomic group."),
            p(i(
              "View taxonomic variation ",
              a(href := "#/gene/http://zfin.org/ZDB-GENE-070117-2105?tab=similarity", "similar to the phenotypic profile of the gene", b("apa")), "."))),
          examplePanel(
            "View presence and absence of any anatomical structure across taxa",
            p("Using the OntoTrace reasoning framework, the KB infers presence of absence of anatomical structure from both direct and indirect statements."),
            p(i(
              "The pectoral fin is known to be ",
              a(href := "#/entity/http://purl.obolibrary.org/obo/UBERON_0000151?tab=taxa&taxatab=presence&genestab=phenotypes", "present in some taxa"),
              ", but ",
              a(href := "#/entity/http://purl.obolibrary.org/obo/UBERON_0000151?tab=taxa&taxatab=absence&genestab=phenotypes", "absent in others"),
              ". You can also view the phenotypic assertions leading to an inference, such as, ",
              a(
                href := "#/taxon/http://purl.obolibrary.org/obo/VTO_0058743?tab=phenotypes&phenotypes.quality_type=entailing-absence&phenotypes.entity=http:%2F%2Fpurl.obolibrary.org%2Fobo%2FUBERON_0000151",
                "absence of ", b("pectoral fin"), " in ", b("Branchiostoma")),
              "."))),
          examplePanel(
            "Download a character matrix of presence/absence inferred knowledge",
            p("Configure a NeXML character matrix download for any type of anatomical structure and taxonomic group using the ", a(href := " #/ ontotrace", "OntoTrace page"), "."),
            p(i(
              "For example, you can create a ",
              a(
                href := "http://kb.phenoscape.org/api/ontotrace?entity=%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FUBERON_0008897%3E&taxon=%3Chttp%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FVTO_0034953%3E&variable_only=true&parts=true",
                "character matrix of presence/absence variation for any ", b("fin structures and their parts"), " for ", b("Ostariophysan fishes")),
              "."))))),
      div(
        cls := "col-sm-3",
        div(
          cls := "panel panel-default",
          div(
            cls := "panel-body",
            h3("Analysis tools"),
            ul(
              li(a(href := "#/ontotrace", "OntoTrace (anatomical presence/absence inference)")),
              li(a(href := "#/annotate_text", "Annotate text"))),
            h3("Data API"),
            p(
              "This website is built on our publicly accessible web service APIs. Service documentation and automatically generated code examples are available via ",
              a(href := "http://docs.phenoscapekb.apiary.io/", "Apiary"), "."),
            h3("Contribute data"),
            p(
              "Phenoscape welcomes community contributions. Get started by reviewing our ",
              a(href := "http://wiki.phenoscape.org/wiki/Resources_for_Data_Contributors", "resources for data contributors"), ".")))))
  }

  private def examplePanel(heading: VNode, nodes: VNode*): VNode = {
    div(
      cls := "col-sm-4 col-xs-6",
      div(
        cls := "panel panel-default",
        div(
          cls := "panel-heading",
          h5(
            cls := "example-heading",
            heading)),
        div(((cls := "panel-body") +: nodes): _*)))
  }

}