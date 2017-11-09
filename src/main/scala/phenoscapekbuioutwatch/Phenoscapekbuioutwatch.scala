package phenoscapekbuioutwatch

import scala.scalajs.js.JSApp
import outwatch.dom._

object Phenoscapekbuioutwatch extends JSApp {
  def main(): Unit = {
    OutWatch.render("#app", h1("Hello World"))
  }
}
