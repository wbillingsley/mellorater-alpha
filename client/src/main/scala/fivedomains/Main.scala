//> using repository "sonatype:snapshots"
//> using dep "com.wbillingsley::doctacular::0.3.0"

package fivedomains

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.*
import org.scalajs.dom

import scalajs.js
import scala.scalajs.js.annotation._

import installers.installMarked

val root = mount("#app", <.p("Loading..."))

import typings.marked.mod.marked

given markdown:Markup = Markup(marked(_))

/** Marked.js markdown transformer, for text elements of the app */
//given marked:MarkupTransformer[dom.html.Element] = root.installMarked("4.3.0")

/** Stylesheet */
given styleSuite:StyleSuite = StyleSuite()

@main def main = {

  dom.console.info("Site code loaded. Starting Mellorater.")

  styleSuite.install()

  root.render(Router)
}
