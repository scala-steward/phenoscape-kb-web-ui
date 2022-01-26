package org.phenoscape.kb.ui.components

import com.raquo.laminar.api.L._
import org.phenoscape.kb.ui.App.{EntityPage, Page, TaxonPage}
import org.phenoscape.kb.ui.Model.{Classification, IRI, Taxon, Term}
import org.phenoscape.kb.ui.Vocab._
import org.phenoscape.kb.ui.{KBAPI, Vocab}

object Views {

  def popoverPanel(heading: String)(nodes: Modifier[Div]*): HtmlElement =
    div(cls := "popover-panel",
      div(
        h5(cls := "popover-title", heading)
      ),
      div(nodes)
    )

  def termName(iri: IRI): HtmlElement =
    span(title := iri.id, child.text <-- KBAPI.termLabel(iri).map(_.label))

  def taxonName(taxon: Taxon): HtmlElement = {
    val isGenusOrSpecies = taxon.rank.exists(rank => GenusOrSpecies(rank.iri.id))
    var classes = List("taxon-name")
    if (taxon.extinct) classes = "extinct" :: classes
    val genusSpecies = if (isGenusOrSpecies) "genus-species" else ""
    span(cls := classes.mkString(" "), title := taxon.iri.id, span(cls := genusSpecies, taxon.label))
  }

  def classification(data: Classification, termRenderer: Term => HtmlElement): HtmlElement = {
    val superClasses = Signal.fromValue(data.subClassOf.map(term => li(termRenderer(term))))
    val equivalents = Signal.fromValue(data.equivalentTo.map(term => span(" = ", termRenderer(term))))
    val subClasses = Signal.fromValue(data.superClassOf.map(term => li(termRenderer(term))))
    div(
      cls := "classification-level",
      ul(cls := "list-unstyled", children <-- superClasses),
      div(
        cls := "classification-level",
        p(data.label, span(children <-- equivalents)),
        div(cls := "classification-level", ul(cls := "list-unstyled", children <-- subClasses))))
  }

  def formatSynonyms(syns: List[(IRI, String)]): HtmlElement =
    if (syns.isEmpty) i("None")
    else {
      val synNodes = syns.sortBy(_._2.toLowerCase).map {
        case (relation, value) =>
          span(value, " ", span(cls := "synonym-type", s"(${Vocab.synonymTypes(relation)})"))
      }
      val commaSeparated = synNodes match {
        case first :: rest => (rest.foldLeft(List(first)) { case (list, item) => item :: span(", ") :: list }).reverse
        case Nil           => Nil
      }
      span(commaSeparated)
    }

  def termInfoView(iri: IRI): HtmlElement = {
    val term = KBAPI.termInfo(iri)
    div(
      h4(child <-- term.map(_.term.label)),
      dl(
        dt("Synonyms"), dd(child <-- term.map(t => formatSynonyms(t.synonyms))),
        dt("Definition"), dd(child <-- term.map(_.definition.map(span(_)).getOrElse(i("None")))),
        dt("ID"), dd(Vocab.compact(iri).id)))
  }

  def entityInfoView(iri: IRI, updates: WriteBus[Page]): HtmlElement = {
    val term = KBAPI.termInfo(iri)
    div(
      h4(child <-- term.map(_.term.label)),
      dl(
        dt("Synonyms"), dd(child <-- term.map(t => formatSynonyms(t.synonyms))),
        dt("Definition"), dd(child <-- term.map(_.definition.map(span(_)).getOrElse(i("None")))),
        dt("ID"), dd(Vocab.compact(iri).id)),
      p(a(
        role := "button",
        onClick.mapTo(EntityPage(iri)) --> updates,
        s"View details for ", span(child <-- term.map(_.term.label)))))
  }

  def taxonInfoView(iri: IRI, updates: WriteBus[Page]): HtmlElement = {
    val term = KBAPI.taxon(iri)
    div(
      h4(child <-- term.map(taxonName)),
      dl(
        //dt("Synonyms"), dd(child <-- term.map(t => formatSynonyms(t.))), //FIXME add synonyms to taxon model
        dt("ID"), dd(Vocab.compact(iri).id)),
      p(a(
        role := "button",
        onClick.mapTo(TaxonPage(iri)) --> updates,
        s"View details for ", span(child <-- term.map(taxonName)))))
  }

  def publicationInfoView(iri: IRI): HtmlElement = {
    val study = KBAPI.studyInfo(iri)
    div(
      h4(child <-- study.map(_.label)),
      dl(
        dt("Citation"), dd(child <-- study.map(_.citation)),
        dt("ID"), dd(Vocab.compact(iri).id)))
  }

  def pagination(currentPage: Signal[Int], newPage: Observer[Int], totalPages: Signal[Int]): HtmlElement = {
    val currentAndTotal = Signal.combine(currentPage, totalPages)
    val $ul = for {
      cAndT <- currentAndTotal
      (current, total) = cAndT
      previous = current - 1
      next = current + 1
      onFirstPage = current == 1
      onLastPage = current == total
    } yield {
      ul(
        cls := "pagination pagination-sm",
        li(cls.toggle("disabled") := onFirstPage, a(role := "button", onClick.mapTo(1) --> newPage, "First")),
        li(cls.toggle("disabled") := onFirstPage, a(role := "button", onClick.mapTo(previous) --> newPage, "Previous")),
        li(cls := "active", a(child <-- currentPage.map(_.toString))),
        li(cls.toggle("disabled") := onLastPage, a(role := "button", onClick.mapTo(next) --> newPage, "Next")),
        li(cls.toggle("disabled") := onLastPage, a(role := "button", onClick.mapTo(total) --> newPage, "Last")))
    }
    nav(child <-- $ul)
  }

  val loading: HtmlElement = img(src := "/img/ajax-loader.gif") //FIXME verify path

  def autocompleteField[T](search: String => EventStream[List[T]], selection: EventStream[Option[T]], show: T => String, makeSelection: Observer[Option[T]], placeholderText: Option[String], isDisabled: Observable[Boolean]): HtmlElement = {
    val enteredText = new EventBus[String]()
    // val selectedIndex = createHandler[Int](0)

    val events = (for {
      text <- enteredText.events.filter(_.length > 2).delay(300)
      matches <- search(text)
    } yield matches)

    val currentMatches = EventStream.merge(events, selection.map(_ => Nil))
    val hideDropdown = currentMatches.map(_.isEmpty)

    val selectedAsText = selection.map(_.map(show).getOrElse(""))
    val keyHandler = new EventBus[String]()
    //
    //    keyHandler.filter(_ == "down").combineLatestWith(selectedIndex) { (_, index) =>
    //      index + 1
    //    }
    //    val currentIndex = selectedIndex.merge(hideDropdown.map(_ => 0), keyHandler.filter(_ == "down").combineLatestWith(selectedIndex) { (_, index) =>
    //      index + 1
    //    })
    val shouldFocus = Var[Boolean](false) //
    val ulDisplay = hideDropdown.map(if (_) "none" else "block")
    //val ulCSS = Util.observableCSS(Observable.of("autocomplete-menu" -> true, "dropdown-menu" -> true).merge(hideDropdown.map("live" -> !_)))
    //keyHandler.map(project)

    def listItem(item: T, index: Int): HtmlElement = li(
      onClick.mapTo(Some(item)) --> makeSelection,
      //selected <-- currentIndex.map(_ == index),
      //focus(index) --> selectedIndex,
      role := "option",
      tabIndex := 0,
      a(show(item)))

    // div bool classes: has-success has-feedback
    div(
      cls := "form-group",
      styleAttr := "position: relative;",
      input(
        tpe := "text",
        cls := "form-control",
        onInput.mapToValue --> enteredText,
        value <-- selectedAsText,
        onChange.mapToValue.filter(_.isEmpty).mapTo(None) --> makeSelection,
        autoComplete := "off",
        disabled <-- isDisabled,
        placeholder := placeholderText.getOrElse("")),
      ul(
        onKeyDown.filter(_.keyCode == 38).mapTo("down") --> keyHandler,
        cls := "autocomplete-menu dropdown-menu",
        cls.toggle("live") <-- hideDropdown.map(!_),
        role := "menu",
        styleAttr := "position: absolute;",
        children <-- currentMatches.map(_.zipWithIndex.map { case (item, i) => listItem(item, i) })))
  }

}