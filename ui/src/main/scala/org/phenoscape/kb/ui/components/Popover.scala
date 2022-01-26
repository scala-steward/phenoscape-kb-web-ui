package org.phenoscape.kb.ui.components

import com.raquo.laminar.api.L._
import org.scalajs.dom.raw.Element

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

object Popover {

  object popup {

    private def makeTippy(ref: Element): Tippy = Global.tippy(ref, js.Dynamic.literal(allowHTML = true, interactive = true, theme = "light-border"))

    def :=(content: HtmlElement): Mod[HtmlElement] =
      new Mod[HtmlElement] {
        override def apply(element: HtmlElement): Unit = {
          val tpy = makeTippy(element.ref)
          // the content must be inserted into the dom before providing to tippy, or else events within it won't work
          element.amend(content)
          tpy.setContent(content.ref)
        }
      }

    def :=(content: String): Mod[HtmlElement] = :=(span(content))

    def <--($content: Signal[HtmlElement]): Mod[HtmlElement] =
      new Mod[HtmlElement] {
        override def apply(element: HtmlElement): Unit = {
          val tpy = makeTippy(element.ref)
          val observer = Observer[HtmlElement] { next =>
            // the content must be inserted into the dom before providing to tippy, or else events within it won't work
            element.amend(next)
            tpy.setContent(next.ref)
          }
          element.amend($content --> observer)
        }
      }

    def <--($content: EventStream[HtmlElement]): Mod[HtmlElement] = <--($content.toSignal(span("Loading...")))

  }

  @js.native
  private trait Tippy extends js.Object {

    def setContent(x: Element): Unit = js.native

    def popperInstance: Popper = js.native

  }

  @js.native
  private trait Popper extends js.Object {

    def update(): Unit = js.native

  }

  @js.native
  @JSGlobalScope
  private object Global extends js.Object {

    def tippy(e: Element, props: Object): Tippy = js.native

  }

}
