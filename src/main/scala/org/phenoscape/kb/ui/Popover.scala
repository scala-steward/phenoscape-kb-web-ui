package org.phenoscape.kb.ui

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

import org.scalajs.dom.raw.Element
import org.scalajs.dom.raw.Node

import outwatch.Sink
import outwatch.dom._

object Popover {

  val simplePopover: VDomModifier = insert --> Sink.create[Element] { el =>
    JQuery(el).popover()
  }

  val complexPopover: VDomModifier = insert --> Sink.create[Element] { el =>
    val options = PopoverOptions()
    val jQueryEl = JQuery(el)
    val popoverContent = jQueryEl.children(".popover-element")
    options.content = () => popoverContent
    jQueryEl.popover(options)
    jQueryEl.on("show.bs.popover", () => {
      popoverContent.attr("hidden", false)
    })
  }

  def popup(popupView: => VNode, placement: String, trigger: String)(args: VDomModifier*): VNode = { //FIXME placement should be enum
    val loadPopup = createBoolHandler(false)
    val termInfoView = loadPopup.distinctUntilChanged.map {
      case true  => popupView
      case false => div()
    }
    val fullArgs = Seq(
      span(hidden := true, cls := "popover-element", child <-- termInfoView),
      mouseenter(true) --> loadPopup,
      data.toggle := "popover", data.trigger := trigger, data.placement := placement, data.container := "body", data.html := true,
      Popover.complexPopover,
      tabindex := 0,
      role := "button") ++ args
    a(fullArgs: _*)
  }

  @js.native
  private trait PopoverOptions extends js.Object {
    var content: js.Function = js.native
  }

  private object PopoverOptions {

    def apply(): PopoverOptions =
      js.Dynamic.literal().asInstanceOf[PopoverOptions]

  }

  @js.native
  private trait JQuery extends js.Object {

    def popover(x: Any*): Unit = js.native
    def on(event: String, func: js.Function): Unit = js.native
    def children(selector: String): JQuery = js.native
    def attr(key: String, value: Any): Unit = js.native
  }

  @js.native
  @JSGlobal("jQuery")
  private object JQuery extends js.Object {
    def apply(x: Node): JQuery = js.native
    def apply(x: String): JQuery = js.native
  }

}
