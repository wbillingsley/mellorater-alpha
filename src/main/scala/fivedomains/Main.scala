//> using repository "sonatype:snapshots"
//> using dep "com.wbillingsley::doctacular::0.3.0"

package fivedomains

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.*
import org.scalajs.dom

import scalajs.js
import scala.scalajs.js.annotation._

import installers.installMarked

val root = mount("#render-here", <.p("Loading..."))

/** Marked.js markdown transformer, for text elements of the app */
given marked:MarkupTransformer[dom.html.Element] = root.installMarked("4.3.0")

/** Stylesheet */
given styleSuite:StyleSuite = StyleSuite()

@main def main = {
  styleSuite.install()
  root.render(Router)
}
