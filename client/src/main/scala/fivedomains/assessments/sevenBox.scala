package fivedomains.assessments

import com.wbillingsley.veautiful.*
import html.{Styling, VHtmlContent, DHtmlComponent, EventMethods}

import fivedomains.{given, *}
import model.*

def nextRandomChar():Char = (scala.util.Random.nextInt(0xc7ff) + 0x1000).toChar

def randomHtmlId() = Seq.fill(10)(nextRandomChar()).mkString

val alignLeftStyle = Styling("text-align: left;").register()
val alignRightStyle = Styling("text-align: right;").register()
val alignCentreStyle = Styling("text-align: center;").register()

val stickyTop = Styling("position: sticky; top: 0;").register()
val bgWhite = Styling("background: white;").register()

/**
  * The "seven box" diagram contains the six manually surveyed domains (as rectangles), with the 
  * mental domain superimposed as a circle in the middle.
  * 
  * This SVG diagram format is also used to generate the logo for the app.
  */
def rosetteAndSides(rosette:svg.DSvgContent)(data: Map[Domain, (String, svg.DSvgContent)]) = 
    import svg.*

    val hgap = 10
    val gaps = Seq(10, 10)
    val boxW = 500
    val boxH = Seq(104, 146, 104)
    val boxVPad = Seq(2, 23, 2)
    val circleR = 170

    val width = 2 * boxW + hgap // Width of the stack of rectangles
    val height = boxH.sum + gaps.sum // Height of the stack of rectangles
    val centreX = width / 2 // centre of the stack of rectangles
    val centreY = height / 2 // centre of the stack of rectangles

    val svgTop = Math.min(0, centreY - circleR - 5)
    val svgHeight = Math.max(height, 2 * circleR + 10)

    // The central box fits into the central circle
    val centreH = 100
    val centreW = (Math.sqrt(Math.pow(circleR, 2) - Math.pow(centreH / 2, 2)) * 2).toInt

    def emptyContent = SVG.g()
    val emptyCol = "gainsboro" // the colour of an empty domain track

    /** Aligns boxes on top of each other. */
    def stackBoxes(data: Seq[(Domain, (String, DSvgContent))], left:Boolean) = {
        (for 
            ((domain, (col, content)), index) <- data.zipWithIndex            
        yield g(^.attr.transform := s"translate(${(if left then 0 else boxW + hgap)}, ${boxH.slice(0, index).sum + gaps.slice(0, index).sum })",
            sparkbox(col, boxW, boxH(index))(
                ^.attr("x") := 0,//(if left then 0 else boxW + gap), 
                ^.attr("y") := 0, //(index * (boxH + gap)),
                // ^.attr.style := s"fill: $cream"
            ),
            domainLogoSvg(domain)(^.style := s"fill: $darkCream; font-size: 55px; dominant-baseline: central; text-anchor: middle;",  
                ^.attr.y := boxH(index) / 2, 
                ^.attr.x := (if left then 40 else boxW - 40)
            ),
            g(^.attr.transform := s"translate(${if left then boxW - 180 else 180}, ${boxVPad(index)}) scale(${if left then -1 else 1}, 1) ",
                content
            )
            // foreignObject(
            //     ^.attr("x") := (if alignLeft then 80 else boxW + gap + boxH), 
            //     ^.attr("y") := (index * (boxH + gap)), 
            //     ^.cls := (if alignLeft then alignLeftStyle else alignRightStyle),
            //     ^.attr("width") := boxW - boxH, ^.attr("height") := boxH, content),
        ))
    }

    def stackMask(alignLeft:Boolean) = {
        for 
            index <- 0 until 3
        yield 
            sparkbox("white", boxW, boxH(index))(
                ^.attr("x") := (if alignLeft then 0 else boxW + hgap), 
                ^.attr("y") := (boxH.slice(0, index).sum + gaps.slice(0, index).sum),
            )
    }

    
    val maskId = randomHtmlId()
    /** Masks the SVG, so that the gaps between the boxes let the background show through, rather than white */
    def mask = 
        SVG("mask")(
            ^.attr("id") := maskId,
            stackMask(true),
            stackMask(false),
            circle(^.attr("fill") := "white", ^.attr("cx") := centreX, ^.attr("cy") := centreY, ^.attr("r") := circleR, ^.attr("stroke") := "black", ^.attr("stroke-width") := 7),
        )


    svg(^.attr("viewBox") := s"0 $svgTop $width $svgHeight",
        mask,

        // (for d <- Domain.values yield domainSymbol(d)),

        g(^.attr("mask") := s"url(#$maskId)",
            stackBoxes(
                for d <- Seq(Domain.Nutrition, Domain.Environment, Domain.Health) yield d -> data.getOrElse(d, (emptyCol, emptyContent)),
                true
            ),
            stackBoxes(
                for d <- Seq(Domain.InteractionsEnvironment, Domain.InteractionsSocial, Domain.InteractionsHuman) yield d -> data.getOrElse(d, (emptyCol, emptyContent)),
                false
            ),
            g(^.attr.transform := s"translate($centreX, $centreY)",
              rosette            
            )
        )
    )


/** A sevenBox containing just the text of one survey */
def scoringRose(assessments:Seq[Assessment]) =
    import html.* 

    val ros = assessments.headOption match {
        case Some(a) => 
            rosette(((for 
                d <- Domain.scoredDomains 
            yield 
                d -> (Seq(^.style := s"fill: ${scoreColor(a.categoryScore(d))}"), Seq(domainLogoSvg(d, classes="domain-rosette-logo")))
            ) :+ (
                Domain.Mental -> (Seq(^.style := s"fill: $cream"), Seq(colouredScoreFace(a.overallScore)(^.attr.width := 100, ^.attr.height := 100, ^.attr.x := -50, ^.attr.y := -50)))            
            )).toMap)
        case None => 
            rosette(((for 
                d <- Domain.scoredDomains 
            yield 
                d -> (Seq(^.style := s"fill: $darkCream"), Seq(domainLogoSvg(d, classes="domain-rosette-logo")))
            ) :+ (
                Domain.Mental -> (Seq(^.style := s"fill: $cream"), Seq(neutral("darkCream")(^.attr.width := 100, ^.attr.height := 100, ^.attr.x := -50, ^.attr.y := -50)))
            )).toMap)

    }

    rosetteAndSides(ros)((for d <- Domain.values yield d -> (cream, domainTrack(d, assessments))).toMap)(^.style := "")


/** Used to show a scoring rose as an assessment is being recorded */
def scoringInProgress(assessment:Assessment, activeDomain:Domain) = 
    import html.* 

    val ros = rosette(((for 
                d <- Domain.scoredDomains 
            yield 
                if d == activeDomain then 
                    d -> (Seq(^.style := s"fill: cornflowerblue;"), Seq(domainLogoSvg(d, classes="domain-rosette-logo")))
                else 
                    d -> (Seq(^.style := s"fill: ${scoreColor(assessment.categoryScore(d))}"), Seq(domainLogoSvg(d, classes="domain-rosette-logo")))
            ) :+ (
                Domain.Mental -> (Seq(^.style := s"fill: $cream"), Seq.empty)            
            )).toMap)

    rosetteAndSides(ros)((for d <- Domain.values yield d -> (if d == activeDomain then cream else cream, domainTrack(d, Seq(assessment)))).toMap)(^.style := "")            




val rosetteStyling = Styling(
    """|
       |""".stripMargin
).modifiedBy(
    " .domain-rosette-logo" -> "fill: white; font-size: 55px; text-anchor: middle; dominant-baseline: central;",
    "  .segment-path" -> "stroke: white; stroke-width: 7",
    "  .segment-path.dimmed" -> s"fill: $cream"
    // " .segment" -> "filter: drop-shadow(2px 2px 4px rgba(0, 0, 0, 0.7));",
    // " .mental" -> "filter: drop-shadow(4px 4px 8px rgba(0, 0, 0, 0.7));",
).register()


def rosette(content:Map[Domain, (Seq[svg.DSvgModifier], Seq[svg.DSvgModifier])]):svg.DSvgContent = 
    import svg.* 
    g(^.cls := rosetteStyling,
        for 
            (d, i) <- Seq(Domain.InteractionsEnvironment, Domain.InteractionsSocial, Domain.InteractionsHuman, Domain.Health, Domain.Environment, Domain.Nutrition).zipWithIndex
            (seg, cont) = content(d)
        yield rosetteSegment(i)(seg*)(cont*),

        for (seg, cont) <- content.get(Domain.Mental) yield g(circle(^.attr.r := "80", ^.cls := "mental")(seg*), g(cont*))
    )


def domainTrack(d:Domain, assessments:Seq[Assessment]):svg.DSvgContent = 
    import svg.* 
    
    // How many assessments to show in the grid
    val subset = assessments.take(8)
    val leftToRight = Domain.interactionDomains.contains(d)
    
    var x = 0 // if leftToRight then 0 else 100
    val cellSize = 20
    val cellGap = 5
    val y0 = 15 /* d match {
        case Domain.Nutrition | Domain.InteractionsEnvironment => 50 - cellSize / 2
        case Domain.Environment | Domain.InteractionsSocial | Domain.Mental => cellSize + cellGap + 50 - cellSize / 2 
        case Domain.Health | Domain.InteractionsHuman => 2 * (cellSize + cellGap) + 50 - cellSize / 2 
    }*/

    val xIncr = cellGap + cellSize //if leftToRight then cellGap + cellSize else -cellGap - cellSize
    var alpha = 1d
    g(
        (for a <- subset yield
            x = if x == xIncr then x + xIncr + xIncr/2 else x + xIncr
            alpha = alpha * 0.85
            for (ans, i) <- a.answersInDomain(d).zipWithIndex yield {
                rect(^.attr.x := x, ^.attr.y := y0 + i * (cellSize + cellGap), ^.attr.width := cellSize, ^.attr.height := cellSize, ^.attr.rx := 5,
                ^.style := f"fill: ${scoreColor(Some(ans.value.asDouble))}; opacity: $alpha%2.1f;"
                
                )
            }
        ).flatten,

        if assessments.length > 1 then 
            line(
                ^.attr.x1 := 2 * xIncr + xIncr/8, ^.attr.x2 := 2 * xIncr + xIncr/8, ^.attr.y1 := y0 - cellGap, ^.attr.y2 := y0 + 3 * (cellSize + cellGap), 
                ^.attr.style := s"stroke: $darkCream; stroke-width: 2px;"
            )
        else None
    )



/** 
 * A one-sixth slice of a rosette 
 */
def rosetteSegment(num:Int)(mods: svg.DSvgModifier*)(content: svg.DSvgModifier*) =
    val rotate = 60 * num 
    val rad = Math.PI / 3
    val sin = Math.sin(rad)
    val cos = Math.cos(rad)
    val innerR = 70
    val outerR = 170

    val centerPoint = 125

    val cx = Math.sin(Math.PI / 6) * centerPoint 
    val cy = -Math.cos(Math.PI / 6) * centerPoint

    import svg.*
    g(^.cls := "segment", 
        ^.attr.transform := s"rotate($rotate)",

        // a donut segment rotated
        path(^.cls := "segment-path",
            ^.attr.d := f"M 0 ${-outerR} A $outerR $outerR 0 0 1 ${outerR * sin + 0.5}%2.0f ${-outerR * cos + 0.5}%2.0f L ${innerR * sin + 0.5}%2f ${-innerR * cos + 0.5}%2f A $innerR $innerR 0 0 0 0 ${-innerR} z",
            
        )(mods*),
        //line(^.style := "stroke: rgba(255, 255, 255, 1); stroke-width: 7;", ^.attr.y1 := innerR, ^.attr.y2 := -outerR, ^.attr.x1 := 0, ^.attr.x2 := 0),

        // rotate the content back to horizontal, about a mid-point in the segment
        g(^.attr.transform := f"translate(${cx}%2.0f, ${cy}%2.0f) rotate(${-rotate})")(content*)
    )