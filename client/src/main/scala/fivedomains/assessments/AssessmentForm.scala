package fivedomains.assessments

import com.wbillingsley.veautiful.*
import html.{Styling, VHtmlContent, DHtmlComponent, <, ^, EventMethods}

import fivedomains.{given, *}
import model.*
import typings.std.stdStrings.s

val surveyQstyle = Styling(
    """"""
).modifiedBy(
    " label.sa,label.sd" -> "display: inline-block; width: 50px; margin: 0.5em;",
    " input" -> "width: 40%; margin: 2em;",
).register()


/** Opens and closes the confidence slider */
def confidenceButton(confidence: => Confidence, state:PushVariable[FooterSelection]) = {
    import html.{<, ^}

    <.button(^.cls := (button, if state.value == FooterSelection.Confidence then "selected" else ""),
        <.div(
            <.div("Confidence", ^.style := "font-size: 0.8em;"), 
            <.div(f" ${confidence.value * 100}%2.0f", "%", ^.style := s"color: ${confidence.colourStyle}"),
        ), 
        ^.onClick --> { if state.value == FooterSelection.Confidence then state.value = FooterSelection.None else state.value = FooterSelection.Confidence }
    )
}

val confidenceSliderStyle = Styling(
    """|
       |""".stripMargin
).modifiedBy(
    " input[type=range]::-webkit-slider-thumb" -> "background: orange;",
)

/**
 * Which of the footer elements (e.g. confidence) is currently open
 */
enum FooterSelection:
    case None
    case Confidence

/** A slider for confidence */
def confidenceSlider(confidence:Confidence)(update: (Confidence) => Unit) = {
    import html.{<, ^}

    <.div(^.style := "text-align: center", ^.cls := confidenceSliderStyle,
        <.label(^.cls := "conf", "Confidence"),
        <.div(
            <.label(^.cls := "sd", "Low"),
            <.input(^.attr("type") := "range", 
                ^.prop.value := (confidence.value * 100).floor.toString,
                ^.prop.min := "0", ^.prop.max := "100", ^.prop.step := "1",
                ^.onInput ==> { (e) => for v <- e.inputValue do update(Confidence(v.toDouble / 100)) }
            ),
            <.label(^.cls := "sa", "High"),
        ),
    )
}



/** A slider for strongly agree / strongly disagree */
def stronglyAgreeSlider(value:AnswerValue.Numeric)(update: (AnswerValue) => Unit) = {
    import html.*
    <.div(^.style := "text-align: center",
        <.label(^.cls := "sd", "Strongly disagree"),
        <.input(^.attr("type") := "range", 
            ^.prop("value") := value.value.toString,
            ^.onInput ==> { (e) => for v <- e.inputValue do update(AnswerValue.Numeric(v.toDouble)) }
        ),
        <.label(^.cls := "sa", "Strongly agree"),
    )
}

/** A picker for Very Poor to Very Good */
def ratingPicker(value:AnswerValue.Rated)(update: (AnswerValue) => Unit) = {
    import html.*
    <.div(^.style := "text-align: center",
        <.label(^.cls := "sd", "Very Poor"),

        for level <- Rating.values yield
            if value.value == level then
                <.button(^.attr.disabled := "disabled", ^.attr.style := "background: orange",
                level.abbreviation)
            else
                <.button(
                    ^.on.click --> update(AnswerValue.Rated(level)),
                    level.abbreviation
                ),
                
        <.label(^.cls := "sa", "Very Good"),
    )
}

val questionFooterStyle = Styling(
    """
      |display: flex;
      |flex-direction: row;
      |justify-content: space-between;
      |align-items: center;
      |margin: 1em;
      |""".stripMargin
).modifiedBy().register()



case class AssessmentForm(animal:Animal) extends DHtmlComponent {
    import html.{<, ^}

    val answers = stateVariable(
        (for q <- flattenedQs yield q.num -> q.defaultAnswer).toMap
    )

    val footerSelectors = (for q <- flattenedQs yield q.num -> stateVariable[FooterSelection](FooterSelection.None)).toMap

    // Animates scrolling the next question to the top of the screen
    // 20 is an arbitrary offset so the question isn't hard against the top border
    def scrollQIntoView(num:Int):Unit = 
        for e <- domNode do
            val start = org.scalajs.dom.window.scrollY
            val target = e.querySelector(s"#question$num").asInstanceOf[org.scalajs.dom.html.Element].getBoundingClientRect().y + start - 20 
            animateProperty(start, target, 10) { (i:Double) => 
                org.scalajs.dom.window.scrollTo(0, i.toInt)
            }

    val maxQNum = 18 // TODO: Don't hardcode this

    def submit():Unit = 
        DataStore.addAssessment(Assessment(animal.id, new scalajs.js.Date().valueOf, answers.value))
        Router.routeTo(AppRoute.Animal(animal.id))

    def render = 
      <.div(
        leftBlockHeader(
            Router.path(AppRoute.Front),
            "Assessment",
            <.label(^.cls := (animalName), animal.name)
        ),

        <.p(^.style := "margin: 1em;",
            s"Thinking of ${animal.name}, what is your level of agreement with each of the following statements",
        ),

        <.div(
            for (domain, questions) <- allQuestions yield 
                val dc = domain.color
                <.div(^.cls := (surveyQstyle), 
                    <.div(^.style := s"padding: 5px 1em; background: $dc",
                        <.label(^.style := "color: white", domain.title),
                    ),
                    
                    for q <- questions yield
                        val ans = answers.value.get(q.num).getOrElse(q.defaultAnswer)

                        <.div(^.style := s"border-bottom: 1px solid $dc", ^.attr("id") := s"question${q.num}",
                            <.p(^.style := "margin: 1em;", q.text(animal)),
                            
                            ans.value match {
                                case AnswerValue.Numeric(v) => 
                                    stronglyAgreeSlider(AnswerValue.Numeric(v)) { v => answers.value = answers.value.updated(q.num, ans.copy(value = v)) }
                                case AnswerValue.Rated(v) => 
                                    ratingPicker(AnswerValue.Rated(v)) { v => answers.value = answers.value.updated(q.num, ans.copy(value = v)) }
                            },                            

                            <.div(^.style := "text-align: center", {
                                footerSelectors(q.num).value match {
                                    case FooterSelection.None => Seq()
                                    case FooterSelection.Confidence => 
                                        confidenceSlider(ans.confidence) { c => answers.value = answers.value.updated(q.num, ans.copy(confidence = c)) }

                                }

                            }),

                            // Mid-space controls

                            <.p(^.style := "margin: 1em;", " "),

                            // Footer controls
                            <.div(^.cls := questionFooterStyle,
                                confidenceButton(answers.value(q.num).confidence, footerSelectors(q.num)),

                                if q.num < maxQNum then <.div(^.style := "text-align: right; margin: 1em;",
                                    <.button(^.cls := (button), "Next ↓", ^.onClick --> scrollQIntoView(q.num + 1))
                                ) else Seq()                 
                            )

                            
                        )


                )

        ),

        <.div(^.style := "text-align: right; margin: 1em;",
            <.button(^.cls := (button, primary), "Submit", ^.onClick --> submit())
        )

    )
}

/**
 * The assessment page for a given animal
 */
def assessmentPage(aId: AnimalId) = 
    val a = DataStore.animal(aId)
    assessments.AssessmentForm(a)
