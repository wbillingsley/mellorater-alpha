package fivedomains

import com.wbillingsley.veautiful.*
import html.{VHtmlContent, Styling, DHtmlComponent, Animator}

import model.*

val darkGreen = "#4b7b55"
val lightGreen = "#82ac8c"
val orange = "#f8ba52"
val lavender = "#cca5cd"
val darkPurple = "#713291"

val cream = "#f5f2e3"
val darkCream = "#d4c787" // cream darkened by 25%

val headerFontFamily = "Lexend, sans-serif"

val veryPoor = darkPurple // "hsl(5, 84%, 70%)"
val poor = lavender // "hsl(9, 86%, 80%)"
val neutral = orange // "hsl(32, 95%, 76%)"
val good = lightGreen //"hsl(120, 27%, 82%)"
val veryGood = darkGreen //"hsl(124, 29%, 71%)"


val fgVeryPoor = "hsl(5, 84%, 30%)"
val fgPoor = "hsl(9, 86%, 30%)"
val fgNeutral = "hsl(32, 95%, 36%)"
val fgGood = "hsl(120, 27%, 32%)"
val fgVeryGood = "hsl(124, 29%, 31%)"


/** Which colour to mark up a score in */
def scoreColor(x:Option[Double]) = x match
    case None => 
        "lightGray"

    case Some(x) =>
        if x < 20 then veryPoor
        else if x < 40 then poor
        else if x < 60 then neutral
        else if x < 80 then good
        else veryGood

def scoreColorClassName(x:Option[Double]) = x match
    case None => "lightGray"
    case Some(x) => 
        if x < 20 then "darkPurple"
        else if x < 40 then "lavender"
        else if x < 60 then "orange"
        else if x < 80 then "lightGreen"
        else "darkGreen"
        
def colouredScoreFace(x:Option[Double]) = x match
    case None => 
        // TODO: Find a different face for unscored
        unhappy(scoreColorClassName(x))

    case Some(v) => 
        if v < 40 then 
            unhappy(scoreColorClassName(x))
        else if v < 60 then 
            neutral(scoreColorClassName(x))
        else 
            happy(scoreColorClassName(x))

def creamFace(x:Option[Double]) = x match
    case None => 
        neutral("lightGray")
    case Some(x) =>
        if x < 40 then 
            unhappy("cream")
        else if x < 60 then 
            neutral("cream")
        else 
            happy("cream")

def boxedScoreFaceHtml(score:Option[Double]) = 
    import html.* 
    <.div(
        ^.attr.style := s"display: inline-block; background: ${scoreColor(score)}; border: 1px solid $cream; border-radius: 5px; padding: 0;",
        creamFace(score)(^.attr.width := 30, ^.attr.height := 25) 
    )

def unboxedDomainLogo(d:Domain, score:Option[Double]) = 
    import html.* 
    <.div(^.style := s"color: ${scoreColor(score)}",
        domainLogo(d) 
    )

def boxedDomainLogo(d:Domain, score:Option[Double]) = 
    import html.* 
    <.div(
        ^.attr.style := s"display: inline-block; background: ${scoreColor(score)}; border: 1px solid $cream; border-radius: 5px; padding: 0;",
        domainLogo(d) 
    )


val faceStyling = Styling("").modifiedBy(
    " .eye" -> "stroke: none;",
    " .mouth" -> "fill: none; stroke-width: 7; stroke-linecap: round;",
    ".cream .eye" -> s"fill: $cream",
    ".cream .mouth" -> s"stroke: $cream",
    ".darkCream .eye" -> s"fill: $darkCream",
    ".darkCream .mouth" -> s"stroke: $darkCream",
    ".orange .eye" -> s"fill: $orange",
    ".orange .mouth" -> s"stroke: $orange",
    ".lavender .eye" -> s"fill: $lavender",
    ".lavender .mouth" -> s"stroke: $lavender",
    ".darkPurple .eye" -> s"fill: $darkPurple",
    ".darkPurple .mouth" -> s"stroke: $darkPurple",
    ".darkGreen .eye" -> s"fill: $darkGreen",
    ".darkGreen .mouth" -> s"stroke: $darkGreen",
    ".lightGreen .eye" -> s"fill: $lightGreen",
    ".lightGreen .mouth" -> s"stroke: $lightGreen",
    ".lightGray .eye" -> s"fill: lightGray",
    ".lightGray .mouth" -> s"stroke: lightGray",
).register()

def happy(colour:String) = 
    import svg.*
    svg(^.attr.viewBox := "0 0 100 100", ^.cls := (faceStyling, colour),
        circle(^.attr.cx := "30", ^.attr.cy := "38", ^.attr.r := 8, ^.cls := "eye"),
        circle(^.attr.cx := "70", ^.attr.cy := "38", ^.attr.r := 8, ^.cls := "eye"),
        path(^.cls := "mouth", ^.attr.d := "M 25 70 A 50 95 1 0 0 75 70")
    )

def neutral(colour:String) = 
    import svg.*
    svg(^.attr.viewBox := "0 0 100 100", ^.cls := (faceStyling, colour),
        circle(^.attr.cx := "30", ^.attr.cy := "38", ^.attr.r := 8, ^.cls := "eye"),
        circle(^.attr.cx := "70", ^.attr.cy := "38", ^.attr.r := 8, ^.cls := "eye"),
        path(^.cls := "mouth", ^.attr.d := "M 25 70 L 75 70")
    )

def unhappy(colour:String) = 
    import svg.*
    svg(^.attr.viewBox := "0 0 100 100", ^.cls := (faceStyling, colour),
        circle(^.attr.cx := "30", ^.attr.cy := "38", ^.attr.r := 8, ^.cls := "eye"),
        circle(^.attr.cx := "70", ^.attr.cy := "38", ^.attr.r := 8, ^.cls := "eye"),
        path(^.cls := "mouth", ^.attr.d := "M 25 80 A 50 90 1 0 1 75 80")
    )



def domainLogo(d:Domain):VHtmlContent = 
    import html.*
    d match {
        case Domain.Environment => <.span(^.cls := "material-symbols-outlined", "partly_cloudy_day")
        case Domain.Health => <.span(^.cls := "material-symbols-outlined", "ecg_heart")
        case Domain.Nutrition => <.span(^.cls := "material-symbols-outlined", "nutrition")
        case Domain.InteractionsEnvironment => <.span(^.cls := "material-symbols-outlined", "nature")
        case Domain.InteractionsSocial => <.span(^.cls := "material-symbols-outlined", "spoke")
        case Domain.InteractionsHuman => <.span(^.cls := "material-symbols-outlined", "person")
        case Domain.Mental => <.span(^.cls := "material-symbols-outlined", "planner_review")
    }

def domainLogoSvg(d:Domain, classes:String = "") = 
    import html.*
    d match {
        case Domain.Environment => SVG.text(^.cls := "material-symbols-outlined " + classes, "\uf172" /*"partly_cloudy_day" */)
        case Domain.Health => SVG.text(^.cls := "material-symbols-outlined " + classes, "\uf6e9" /* ecg_heart */)
        case Domain.Nutrition => SVG.text(^.cls := "material-symbols-outlined " + classes, "\ue110" /* "nutrition" */)
        case Domain.InteractionsEnvironment => SVG.text(^.cls := "material-symbols-outlined " + classes, "\ue406" /* "nature" */)
        case Domain.InteractionsSocial => SVG.text(^.cls := "material-symbols-outlined " + classes, "\ue9a7" /* "spoke" */)
        case Domain.InteractionsHuman => SVG.text(^.cls := "material-symbols-outlined " + classes, "\ue7fd" /* "person" */)
        case Domain.Mental => SVG.text(^.cls := "material-symbols-outlined " + classes,  "\ue694" /* "planner_review" */)
    }


val smallFont = "15px"
val smallLineHeight  = "18px"
val largeFont = "21px"
val largeLineHeight = "28px"

val frontHeaderHeight = "210px"
val otherHeaderHeight = "120px"

val noticeBg = "hsl(240, 33%, 90%)"
val noticeFg = "hsl(240, 33%, 50%)"

val dangerBg = "hsl(4, 85%, 90%)"
val dangerFg = "hsl(4, 85%, 50%)"


val nakedParaMargins = Styling("margin: 1em").register()

val top = Styling(
    """font-family: sans-serif; max-width: 480px; margin: auto;"""
).modifiedBy(
    " h1,h2,h3,h4,h5,h6 " -> s"font-family: $headerFontFamily",
    " input,button,label" -> s"font-size: $smallFont; line-height: $smallLineHeight;"
).register()

val button = Styling(
    s"font-size: $smallFont; margin-left: 0.5em; border-radius: 0.25em; padding: 0.75em; border: none; text-decoration: none; display: inline-block;"
).modifiedBy(
    ".active" -> "font-weight: bold; background: none; color: black;",
    ".enabled" -> "background-color: aliceblue; color: cornflowerblue;"
).register()

val primary = Styling(
   "color: white; background: cornflowerblue;"
).modifiedBy(
    ":hover" -> "filter: brightness(85%);"
).register()

val card = Styling(
    "box-shadow: 0 3px 3px #aaa; margin: 20px; padding: 20px; font-family: sans-serif;"
).register()

val animalName = Styling(
    s"font-size: $largeFont; font-family: $headerFontFamily, font-weight: bold;"
).register()

val notice = Styling(
    s"font-size: $smallFont; margin: 1em; border-radius: 0.25em; padding: 1em; color: $noticeFg;  max-height: 1000px;"
).register()

val noticeButton = Styling(
   s"color: white; background: $noticeFg;"
).modifiedBy(
    ":hover" -> "filter: brightness(85%);",
        ":disabled,:hover:disabled" -> "filter: opacity(35%);"
).register()

val dangerButton = Styling(
   s"color: white; background: $dangerFg;"
).modifiedBy(
    ":hover" -> "filter: brightness(85%);",
    ":disabled,:hover:disabled" -> "filter: opacity(35%);"
).register()


val fiveboxtext = Styling(
   s"color: white; font-size: 48px; width: 75px; line-height: 100px;"
).modifiedBy(
    
).register()

def fourGrid(a:VHtmlContent, b:VHtmlContent, c:VHtmlContent, d:VHtmlContent):VHtmlContent = 
    import html.{<, ^}
    <.div(^.attr("style") := "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
        a, b, c, d
    )

def sparkbox(color:String, width:Int, height:Int) = 
    import svg.*
    rect(^.attr("width") := width, ^.attr("height") := height, ^.attr("fill") := color)


val fakeSparkLine =
    import svg.* 
    path(
        ^.attr("d") := "M 40 140 c 30 0 60 -80 90 -80 c 30 0 60 60 90 60 c 40 0 80 -60 120 -60 c 40 0 80 60 120 60 c 40 0 60 -60 90 -60 c 40 0 80 60 120 60 c 30 0 60 -80 90 -80",
        ^.attr("style") := "fill: none; stroke: white; stroke-width: 5;"
    )


def imageFile(name:String, alt:String) = 
    import html.* 
    img(^.src := "images/" + name, ^.attr.alt := alt)


def fullLogo = imageFile("logo full.png", "Mellorator logo")



// def logo =
//     import html.* 
//     import assessments.*
//     assessments.sevenBox(
//         (for d <- Domain.values yield d -> (d.color, <.div())).toMap
//     )

/**
  * The header that goes at the top of the front page
  */
def frontHeader = 
    import html.*
    <.div(
        ^.cls := Styling(
            s"margin: 0 0 1.5em; position: relative; top: 0; height: $frontHeaderHeight; box-shadow: 0 3px 3px #aaa;"
        ).modifiedBy(
            " .bgimage" -> """
                |width: 100%;
                |position: absolute; height: 100%; width: 100%; top: 0;
                |background-image: url("images/logo full.png");
                |background-position: center;
                |background-blend-mode: soft-light;
                |background-size: contain;
                |""".stripMargin,
            " .title-block" -> s"text-align: center; position: absolute; bottom: 1em; font-family: sans-serif; font-size: $largeFont; width: 100%;",
            " .five-domains-logo" -> "width: 90px;"
        ).register().className,
        <.div(^.cls := "bgimage"),
        <.div(^.cls := "title-block",
            <.div(^.cls := "title",
            )
        )        
    )

def leftBlockHeader(backurl:String, title:String, block:VHtmlContent, decoration:VHtmlContent = html.<.div()) = 
    import html.{<, ^}
    <.div(
        ^.cls := Styling(
                "margin: 0 0 1.5em; position: relative; top: 0; height: 120px; padding: 20px; box-shadow: 0 3px 3px #aaa; font-family: sans-serif;"
            ).modifiedBy(
                " .bgimage" -> s"""
                  |opacity: 0.5;
                  |position: absolute; height: 100%; width: 100%; top: 0; left: 0;
                  |background-color: $darkCream 
                  |background-position: center;
                  |background-size: cover;
                  |""".stripMargin,
                " .foreground" -> "position: relative; top: 0; left: 0; height: 100%;",
                " .links a" -> s"text-decoration: none; font-size: $smallFont; color: black;",
                " .title-block" -> s"font-size: $largeFont; width: 100%; color: $darkCream; margin-top: 1em;",
                " .block" -> "position: absolute; bottom: 0;",
            ).register().className,

            <.div(^.cls := "bgimage"),
            <.div(^.cls := "foreground",
                <.div(^.cls := "links", <.a(^.href := backurl, "← Back")),
                <.div(^.cls := "title-block", title),
                <.div(^.cls := "block", block),
                <.div(^.cls := "decoration", decoration),
            )
    )



def bg1 = 
    import svg.{<, ^}
    <.svg(
        ^.attr("viewBox") := "0 0 10 10",
        <.circle(^.attr("cx") := 0, ^.attr("cy") := "5", ^.attr("r") := 5, ^.style := "fill: aliceblue;")
    )


val curls = """<svg id='curls' width='100%' height='100%' xmlns='http://www.w3.org/2000/svg'><defs><pattern id='a' patternUnits='userSpaceOnUse' width='35.584' height='30.585' patternTransform='scale(2) rotate(0)'><rect x='0' y='0' width='100%' height='100%' fill='hsla(0,0%,100%,1)'/><path d='M36.908 9.243c-5.014 0-7.266 3.575-7.266 7.117 0 3.376 2.45 5.726 5.959 5.726 1.307 0 2.45-.463 3.244-1.307.744-.811 1.125-1.903 1.042-3.095-.066-.811-.546-1.655-1.274-2.185-.596-.447-1.639-.894-3.162-.546-.48.1-.778.58-.662 1.06.1.48.58.777 1.06.661.695-.149 1.274-.066 1.705.249.364.265.546.645.562.893.05.679-.165 1.308-.579 1.755-.446.48-1.125.744-1.936.744-2.55 0-4.188-1.538-4.188-3.938 0-2.466 1.44-5.347 5.495-5.347 2.897 0 6.008 1.888 6.388 6.058.166 1.804.067 5.147-2.598 7.034a.868.868 0 00-.142.122c-1.311.783-2.87 1.301-4.972 1.301-4.088 0-6.123-1.952-8.275-4.021-2.317-2.218-4.7-4.518-9.517-4.518-4.094 0-6.439 1.676-8.479 3.545.227-1.102.289-2.307.17-3.596-.496-5.263-4.567-7.662-8.159-7.662-5.015 0-7.265 3.574-7.265 7.116 0 3.377 2.45 5.727 5.958 5.727 1.307 0 2.449-.463 3.243-1.308.745-.81 1.126-1.903 1.043-3.095-.066-.81-.546-1.654-1.274-2.184-.596-.447-1.639-.894-3.161-.546-.48.1-.778.58-.662 1.06.099.48.579.777 1.059.66.695-.148 1.275-.065 1.705.25.364.264.546.645.563.893.05.679-.166 1.307-.58 1.754-.447.48-1.125.745-1.936.745-2.549 0-4.188-1.539-4.188-3.939 0-2.466 1.44-5.345 5.495-5.345 2.897 0 6.008 1.87 6.389 6.057.163 1.781.064 5.06-2.504 6.96-1.36.864-2.978 1.447-5.209 1.447-4.088 0-6.124-1.952-8.275-4.021-2.317-2.218-4.7-4.518-9.516-4.518v1.787c4.088 0 6.123 1.953 8.275 4.022 2.317 2.218 4.7 4.518 9.516 4.518 4.8 0 7.2-2.3 9.517-4.518 2.151-2.069 4.187-4.022 8.275-4.022s6.124 1.953 8.275 4.022c2.318 2.218 4.701 4.518 9.517 4.518 4.8 0 7.2-2.3 9.516-4.518 2.152-2.069 4.188-4.022 8.276-4.022s6.123 1.953 8.275 4.022c2.317 2.218 4.7 4.518 9.517 4.518v-1.788c-4.088 0-6.124-1.952-8.275-4.021-2.318-2.218-4.701-4.518-9.517-4.518-4.103 0-6.45 1.683-8.492 3.556.237-1.118.304-2.343.184-3.656-.497-5.263-4.568-7.663-8.16-7.663z'  stroke-width='1' stroke='none' fill='hsla(258.5,59.4%,59.4%,1)'/><path d='M23.42 41.086a.896.896 0 01-.729-.38.883.883 0 01.215-1.242c2.665-1.887 2.764-5.23 2.599-7.034-.38-4.187-3.492-6.058-6.389-6.058-4.055 0-5.495 2.88-5.495 5.346 0 2.4 1.639 3.94 4.188 3.94.81 0 1.49-.265 1.936-.745.414-.447.63-1.076.58-1.755-.017-.248-.2-.629-.547-.893-.43-.315-1.026-.398-1.704-.249a.868.868 0 01-1.06-.662.868.868 0 01.662-1.059c1.523-.348 2.566.1 3.161.546.729.53 1.209 1.374 1.275 2.185.083 1.191-.298 2.284-1.043 3.095-.794.844-1.936 1.307-3.244 1.307-3.508 0-5.958-2.35-5.958-5.726 0-3.542 2.25-7.117 7.266-7.117 3.591 0 7.663 2.4 8.16 7.663.347 3.79-.828 6.868-3.344 8.656a.824.824 0 01-.53.182zm0-30.585a.896.896 0 01-.729-.38.883.883 0 01.215-1.242c2.665-1.887 2.764-5.23 2.599-7.034-.381-4.187-3.493-6.058-6.389-6.058-4.055 0-5.495 2.88-5.495 5.346 0 2.4 1.639 3.94 4.188 3.94.81 0 1.49-.266 1.936-.746.414-.446.629-1.075.58-1.754-.017-.248-.2-.629-.547-.894-.43-.314-1.026-.397-1.705-.248A.868.868 0 0117.014.77a.868.868 0 01.662-1.06c1.523-.347 2.566.1 3.161.547.729.53 1.209 1.374 1.275 2.185.083 1.191-.298 2.284-1.043 3.095-.794.844-1.936 1.307-3.244 1.307-3.508 0-5.958-2.35-5.958-5.726 0-3.542 2.25-7.117 7.266-7.117 3.591 0 7.663 2.4 8.16 7.663.347 3.79-.828 6.868-3.344 8.656a.824.824 0 01-.53.182zm29.956 1.572c-4.8 0-7.2-2.3-9.517-4.518-2.151-2.069-4.187-4.022-8.275-4.022S29.46 5.486 27.31 7.555c-2.317 2.218-4.7 4.518-9.517 4.518-4.8 0-7.2-2.3-9.516-4.518C6.124 5.486 4.088 3.533 0 3.533s-6.124 1.953-8.275 4.022c-2.317 2.218-4.7 4.518-9.517 4.518-4.8 0-7.2-2.3-9.516-4.518-2.152-2.069-4.188-4.022-8.276-4.022V1.746c4.8 0 7.2 2.3 9.517 4.518 2.152 2.069 4.187 4.022 8.275 4.022s6.124-1.953 8.276-4.022C-7.2 4.046-4.816 1.746 0 1.746c4.8 0 7.2 2.3 9.517 4.518 2.151 2.069 4.187 4.022 8.275 4.022s6.124-1.953 8.275-4.022c2.318-2.218 4.7-4.518 9.517-4.518 4.8 0 7.2 2.3 9.517 4.518 2.151 2.069 4.187 4.022 8.275 4.022s6.124-1.953 8.275-4.022c2.317-2.218 4.7-4.518 9.517-4.518v1.787c-4.088 0-6.124 1.953-8.275 4.022-2.317 2.234-4.717 4.518-9.517 4.518z'  stroke-width='1' stroke='none' fill='hsla(339.6,82.2%,51.6%,1)'/></pattern></defs><rect width='800%' height='800%' transform='translate(0,0)' fill='url(%23a)'/></svg>"""
val herringbone = """<svg id='herringbone' width='100%' height='100%' xmlns='http://www.w3.org/2000/svg'><defs><pattern id='a' patternUnits='userSpaceOnUse' width='40' height='40' patternTransform='scale(2) rotate(55)'><rect x='0' y='0' width='100%' height='100%' fill='hsla(185,100%,34.5%,1)'/><path d='M15 5h10v30H15zM35-5V5H5V-5zM35 35v10H5V35zM35-15h10v30H35zM55 15v10H25V15zM15 15v10h-30V15zM35 25h10v30H35zM-5 25H5v30H-5zM-5-15H5v30H-5z'  stroke-width='8' stroke='hsla(46,81.3%,62.4%,1)' fill='none'/></pattern></defs><rect width='800%' height='800%' transform='translate(0,0)' fill='url(%23a)'/></svg>"""
val flowers = """<svg id='flowers' width='100%' height='100%' xmlns='http://www.w3.org/2000/svg'><defs><pattern id='a' patternUnits='userSpaceOnUse' width='30' height='40' patternTransform='scale(3) rotate(155)'><rect x='0' y='0' width='100%' height='100%' fill='hsla(205,11.2%,84.5%,1)'/><path d='M1.624 19.09l6.597-1.595a.503.503 0 11.238.98L2.145 20l6.314 1.526a.504.504 0 01-.238.98l-6.597-1.595 3.426 3.426a3.813 3.813 0 005.386 0l1.1-1.1a4.584 4.584 0 000-6.475l-1.1-1.099a3.814 3.814 0 00-5.386 0zM-.911 18.377l-1.595-6.597a.504.504 0 11.98-.237L0 17.856l1.526-6.313a.503.503 0 11.98.237L.911 18.377l3.426-3.426a3.813 3.813 0 000-5.386l-1.1-1.099A4.548 4.548 0 000 7.125a4.547 4.547 0 00-3.238 1.341l-1.099 1.099a3.813 3.813 0 000 5.386zM-11.535 16.763a4.584 4.584 0 000 6.476l1.1 1.099a3.813 3.813 0 005.385 0l3.426-3.426-6.597 1.595a.501.501 0 01-.609-.371.504.504 0 01.372-.609l6.313-1.526-6.313-1.526a.504.504 0 11.237-.98l6.597 1.595-3.426-3.426a3.796 3.796 0 00-2.693-1.113c-.975 0-1.95.37-2.693 1.113zM.911 21.625l1.595 6.597a.504.504 0 11-.98.237L0 22.146l-1.526 6.313a.505.505 0 01-.98-.237l1.595-6.597-3.426 3.426a3.813 3.813 0 000 5.386l1.1 1.099a4.584 4.584 0 006.475 0l1.099-1.099a3.813 3.813 0 000-5.386zM31.624 19.09l6.597-1.595a.503.503 0 11.238.98L32.145 20l6.314 1.526a.504.504 0 01-.238.98l-6.597-1.595 3.426 3.426a3.813 3.813 0 005.386 0l1.1-1.1a4.584 4.584 0 000-6.475l-1.1-1.099a3.814 3.814 0 00-5.386 0zM29.089 18.377l-1.595-6.597a.504.504 0 11.98-.237L30 17.856l1.526-6.313a.503.503 0 11.98.237l-1.595 6.597 3.426-3.426a3.813 3.813 0 000-5.386l-1.1-1.099A4.548 4.548 0 0030 7.125a4.547 4.547 0 00-3.238 1.341l-1.099 1.099a3.813 3.813 0 000 5.386zM18.465 16.763a4.584 4.584 0 000 6.476l1.1 1.099a3.813 3.813 0 005.385 0l3.426-3.426-6.597 1.595a.501.501 0 01-.609-.371.504.504 0 01.372-.609l6.313-1.526-6.313-1.526a.504.504 0 11.237-.98l6.597 1.595-3.426-3.426a3.796 3.796 0 00-2.693-1.113c-.975 0-1.95.37-2.693 1.113zM30.911 21.625l1.595 6.597a.504.504 0 11-.98.237L30 22.146l-1.526 6.313a.505.505 0 01-.98-.237l1.595-6.597-3.426 3.426a3.813 3.813 0 000 5.386l1.1 1.099a4.584 4.584 0 006.475 0l1.099-1.099a3.813 3.813 0 000-5.386zM16.624 39.09l6.597-1.595a.503.503 0 11.238.98L17.145 40l6.314 1.526a.504.504 0 01-.238.98l-6.597-1.595 3.426 3.426a3.813 3.813 0 005.386 0l1.1-1.1a4.584 4.584 0 000-6.475l-1.1-1.099a3.814 3.814 0 00-5.386 0zM14.089 38.377l-1.595-6.597a.504.504 0 11.98-.237L15 37.856l1.526-6.313a.503.503 0 11.98.237l-1.595 6.597 3.426-3.426a3.813 3.813 0 000-5.386l-1.1-1.099A4.548 4.548 0 0015 27.125a4.547 4.547 0 00-3.238 1.341l-1.099 1.099a3.813 3.813 0 000 5.386zM3.465 36.763a4.584 4.584 0 000 6.476l1.1 1.099a3.813 3.813 0 005.385 0l3.426-3.426-6.597 1.595a.501.501 0 01-.609-.371.504.504 0 01.372-.609l6.313-1.526-6.313-1.526a.504.504 0 11.237-.98l6.597 1.595-3.426-3.426a3.796 3.796 0 00-2.693-1.113c-.975 0-1.95.37-2.693 1.113zM15.911 41.625l1.595 6.597a.504.504 0 11-.98.237L15 42.146l-1.526 6.313a.505.505 0 01-.98-.237l1.595-6.597-3.426 3.426a3.813 3.813 0 000 5.386l1.1 1.1a4.584 4.584 0 006.475 0l1.099-1.1a3.813 3.813 0 000-5.386zM16.624-.91l6.597-1.595a.503.503 0 11.238.98L17.145 0l6.314 1.526a.504.504 0 01-.238.98L16.624.912l3.426 3.426a3.813 3.813 0 005.386 0l1.1-1.1a4.584 4.584 0 000-6.475l-1.1-1.099a3.814 3.814 0 00-5.386 0zM14.089-1.623L12.494-8.22a.504.504 0 11.98-.237L15-2.144l1.526-6.313a.503.503 0 11.98.237l-1.595 6.597 3.426-3.426a3.813 3.813 0 000-5.386l-1.1-1.099A4.548 4.548 0 0015-12.875a4.547 4.547 0 00-3.238 1.341l-1.099 1.099a3.813 3.813 0 000 5.386zM3.465-3.237a4.584 4.584 0 000 6.476l1.1 1.099a3.813 3.813 0 005.385 0L13.376.912 6.779 2.507a.501.501 0 01-.609-.371.504.504 0 01.372-.609L12.855.001 6.542-1.525a.504.504 0 11.237-.98L13.376-.91 9.95-4.336a3.796 3.796 0 00-2.693-1.113c-.975 0-1.95.37-2.693 1.113zM15.911 1.625l1.595 6.597a.504.504 0 11-.98.237L15 2.146 13.474 8.46a.505.505 0 01-.98-.237l1.595-6.597-3.426 3.426a3.813 3.813 0 000 5.386l1.1 1.099a4.584 4.584 0 006.475 0l1.099-1.099a3.813 3.813 0 000-5.386z'  stroke-width='1' stroke='none' fill='hsla(205,14.4%,60.6%,1)'/></pattern></defs><rect width='800%' height='800%' transform='translate(0,0)' fill='url(%23a)'/></svg>"""
val circles = """<svg id='circles' width='100%' height='100%' xmlns='http://www.w3.org/2000/svg'><defs><pattern id='a' patternUnits='userSpaceOnUse' width='25' height='25' patternTransform='scale(3) rotate(90)'><rect x='0' y='0' width='100%' height='100%' fill='hsla(50,100%,71.4%,1)'/><path d='M25 30a5 5 0 110-10 5 5 0 010 10zm0-25a5 5 0 110-10 5 5 0 010 10zM0 30a5 5 0 110-10 5 5 0 010 10zM0 5A5 5 0 110-5 5 5 0 010 5zm12.5 12.5a5 5 0 110-10 5 5 0 010 10z'  stroke-width='4' stroke='hsla(0,100%,71%,1)' fill='none'/><path d='M0 15a2.5 2.5 0 110-5 2.5 2.5 0 010 5zm25 0a2.5 2.5 0 110-5 2.5 2.5 0 010 5zM12.5 2.5a2.5 2.5 0 110-5 2.5 2.5 0 010 5zm0 25a2.5 2.5 0 110-5 2.5 2.5 0 010 5z'  stroke-width='4' stroke='hsla(176,55.9%,55.5%,1)' fill='none'/></pattern></defs><rect width='800%' height='800%' transform='translate(0,0)' fill='url(%23a)'/></svg>"""
val plaid = """<svg id='plaid' width='100%' height='100%' xmlns='http://www.w3.org/2000/svg'><defs><pattern id='a' patternUnits='userSpaceOnUse' width='30' height='30' patternTransform='scale(2) rotate(95)'><rect x='0' y='0' width='100%' height='100%' fill='hsla(55,96.2%,79.4%,1)'/><path d='M18.35 0l-.03 1.67L20 0zm3.3 0l-3.33 3.33.03 1.67 3.33-3.33zm.03 3.33l-3.36 3.34v1.66L21.65 5zm0 3.34L18.35 10l-.03 1.68 3.36-3.35zM21.65 10l-3.33 3.32.03 1.68 3.33-3.32zm.03 3.32l-3.36 3.36v1.64L21.65 15zm-3.36 5L15 21.65l1.68.03L18.35 20l-.03 1.68 1.68-.03-1.68 1.68.03 1.67 3.33-3.32-.03-1.68 1.68-1.68h-1.65v-1.64L20 18.35zm3.36 3.36h1.65l3.34-3.36-1.67.03zM3.33 18.32l-1.68.03L0 20v1.65zm3.34 0L5 18.35l-3.33 3.33h1.66zm1.66 0L5 21.65l1.67.03L10 18.35zm3.35 0l-3.35 3.36 1.67-.03 3.32-3.33zm5 0l-1.68.03-3.32 3.33h1.64zm11.65 0L25 21.65l1.67.03L30 18.35zM30 20l-1.67 1.68 1.67-.03zm-8.32 3.33l-3.36 3.34v1.66L21.65 25zm0 3.34L18.35 30H20l1.68-1.67z'  stroke-width='1' stroke='none' fill='hsla(198,97.1%,41.2%,1)'/><path d='M10 30v-1.65L8.35 30zm-3.35 0L10 26.65V25l-5 5zM10 15L0 25v1.65l10-10zm0 3.35l-10 10V30l10-10zM3.35 30L10 23.35v-1.7L1.65 30zM5 10l-5 5v1.65L6.65 10zm-3.35 0L0 11.65v1.7L3.35 10zM10 10H8.35L0 18.35V20zm0 1.65l-10 10v1.7l10-10zM10 0v10H0V0h10v1.65L11.65 0M20 6.65L16.65 10h1.7L20 8.35 28.35 0h-1.7zM23.35 0L20 3.35 13.35 10H15l5-5 5-5zM30 0L20 10h1.65L30 1.65zm-1.65 10L30 8.35v-1.7L26.65 10zM25 10l5-5V3.35L23.35 10zM16.65 0L10 6.65v1.7L18.35 0zM20 1.65L21.65 0H20L10 10h1.65zM15 0h-1.65L10 3.35V5z'  stroke-width='1' stroke='none' fill='hsla(4,84.5%,57.1%,1)'/></pattern></defs><rect width='800%' height='800%' transform='translate(0,0)' fill='url(%23a)'/></svg>"""

def patternBox(pattern:String) = Styling(
    s"""|background: url("data:image/svg+xml,$pattern"), white;
        |""".stripMargin
)

val patterns = Map(
    DisplayStyle.Curls -> patternBox(curls).register(),
    DisplayStyle.Herringbone -> patternBox(herringbone).register(),
    DisplayStyle.Flowers -> patternBox(flowers).register(),
    DisplayStyle.Circles -> patternBox(circles).register(),
    DisplayStyle.Plaid -> patternBox(plaid).register(),
)


def background(pattern:String) = Styling(
    // s"""|background-image: linear-gradient(to right, rgba(255, 255, 255, 0.5), rgba(255, 255, 255, 0.93) 2%, rgba(255, 255, 255, 0.93) 98%, rgba(255, 255, 255, 0.5)),
    //     |                   url("data:image/svg+xml,$pattern");
    //     |background-position: left, right;
    //     |""".stripMargin, 
    ""
)

val backgrounds = Map(
    DisplayStyle.Curls -> background(curls).register(),
    DisplayStyle.Herringbone -> background(herringbone).register(),
    DisplayStyle.Flowers -> background(flowers).register(),
    DisplayStyle.Circles -> background(circles).register(),
    DisplayStyle.Plaid -> background(plaid).register(),
)

def animateProperty(start:Double, stop:Double, steps:Int)(f:(Double) => Unit) = {
    val step = (stop - start)/steps + 1
    var cursor = start + step

    def set():Unit = 
        if Math.abs(cursor - start) > Math.abs(stop - start) then 
            f(stop)
        else 
            f(cursor)
            cursor = cursor + step
            Animator.queue(_ => set())
    
    Animator.queue(_ => set())
}