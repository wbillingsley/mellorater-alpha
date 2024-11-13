package fivedomains.assessments

import com.wbillingsley.veautiful.*
import html.{Styling, VHtmlContent, DHtmlComponent, <, ^, EventMethods, DHtmlModifier}

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

/** Opens and closes the explanation */
def footerButton(state:PushVariable[FooterSelection], text: DHtmlModifier, mode:FooterSelection) = {
    import html.{<, ^}

    <.button(^.cls := (button, if state.value == mode then "selected" else ""),
        <.div(
            text
        ), 
        ^.onClick --> { if state.value == mode then state.value = FooterSelection.None else state.value = mode }
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
    case Explanation
    case Notes
    case Photo

/** A slider for confidence */
def confidenceSlider(confidence:Confidence)(update: (Confidence) => Unit) = {
    import html.{<, ^}

    <.div(^.style := "text-align: center; margin: 1em;", ^.cls := confidenceSliderStyle,
        <.h4(^.cls := "conf", "Confidence"),
        <.p("This lets you mark cases where there is unusually weak or strong evidence to make a conclusion"),
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
        <.div(^.style := "position; relative; height: 5px; overflow: visible",
            colouredScoreFace(Some(value.value))(^.attr.width := 30, ^.attr.height := 30)
        ),
        <.label(^.cls := "sd", "Strongly disagree"),
        <.input(^.attr("type") := "range", 
            ^.prop("value") := value.value.toString,
            ^.onInput ==> { (e) => for v <- e.inputValue do update(AnswerValue.Numeric(v.toDouble)) }
        ),
        <.label(^.cls := "sa", "Strongly agree"),
    )
}

val likertStyle = Styling(
    """
      |display: flex;
      |justify-content: space-between;
      |align-items: center;
      |margin: 1em;
      |""".stripMargin
).modifiedBy(
    " input[type='radio']" -> "appearance: none; border-radius: 50%; border: 0.15em solid; width: 2em; height: 2em; margin: 0;",
    " input[type='radio'].StronglyDisagree" -> s"border-color: $darkPurple",
    " input[type='radio'].Disagree" -> s"border-color: $lavender",
    " input[type='radio'].Neutral" -> s"border-color: $orange",
    " input[type='radio'].Agree" -> s"border-color: $lightGreen",
    " input[type='radio'].StronglyAgree" -> s"border-color: $darkGreen",
    " input[type='radio']:checked.StronglyDisagree" -> s"background: $darkPurple",
    " input[type='radio']:checked.Disagree" -> s"background: $lavender",
    " input[type='radio']:checked.Neutral" -> s"background: $orange",
    " input[type='radio']:checked.Agree" -> s"background: $lightGreen",
    " input[type='radio']:checked.StronglyAgree" -> s"background: $darkGreen",
).register()

/** A slider for strongly agree / strongly disagree */
def likertScale(name:String, value:AnswerValue.Numeric)(update: (AnswerValue) => Unit) = {
    import html.*
    <.div(^.cls := likertStyle, ^.style := "text-align: center; display: flex; justify-content: space-between",

        <.label(^.cls := "sd", "Strongly disagree"),
        // <.span(,
            (for agr <- Agreement.values.toSeq yield 
                    <.input(^.attr.`type` := "radio", ^.attr.name := name, ^.cls := agr.toString(), if value.agreement == agr then ^.prop.checked := true else ^.prop.checked := false, ^.on.change --> { update(AnswerValue.Numeric((agr.ordinal * 25).toDouble)) }) 
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
                <.button(^.attr.disabled := "disabled", 
                    ^.attr.style := s"background: ${scoreColor(Some(level.value))}; border: 4px solid black; border-radius: 5px; padding: 0;",
                    creamFace(Some(level.value))(^.attr.width := 30, ^.attr.height := 25)    
                )
            else
                <.button(
                    ^.on.click --> update(AnswerValue.Rated(level)),
                    ^.attr.style := s"background: ${scoreColor(Some(level.value))}; border: none; border-radius: 5px; padding: 0;",
                    creamFace(Some(level.value))(^.attr.width := 30, ^.attr.height := 25)
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

    val situation = stateVariable(Situation.DayToDay)

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
        DataStore.addAssessment(Assessment(animal.id, situation.value, new scalajs.js.Date().valueOf, answers.value))
        Router.routeTo(AppRoute.Animal(animal.id))

    def render = 
      <.div(
        leftBlockHeader(
            Router.path(AppRoute.Front),
            "Assessment",
            <.label(^.cls := (animalName), animal.name)
        ),

        <.p(^.style := "margin: 1em;",
            s"Which situation is this assessment for?",
            <.select(^.style := s"margin-left: 0.25em; max-width: 300px; font-size: $largeFont;",
                    ^.on("input") ==> { e => for n <- e.inputValue do situation.value = Situation.fromOrdinal(n.toInt) },

                    for s <- allowableSituations(animal.species) yield 
                        <.option(
                            ^.prop.value := s.ordinal, s.toString,
                            if s.ordinal == situation.value.ordinal then ^.prop.selected := "selected" else None
                        )
                )
        ),


        <.p(^.style := "margin: 1em;",
            s"Thinking of ${animal.name}, what is your level of agreement with each of the following statements",
        ),

        <.div(
            for (domain, questions) <- allQuestions if questions.exists(!_.dontAsk) yield 
                val dc = domain.color
                <.div(^.cls := (surveyQstyle), 
                    <.div(^.style := s"padding: 5px 1em; background: $dc",
                        
                        <.label(^.style := "color: white", domainLogo(domain), domain.title),
                    ),
                    
                    for q <- questions yield
                        val ans = answers.value.get(q.num).getOrElse(q.defaultAnswer)

                        <.div(^.style := s"border-bottom: 1px solid $dc", ^.attr("id") := s"question${q.num}",
                            <.div(^.style := "margin: 1em;",
                                <.h4(q.headline(animal)),
                                // <.p(q.shortExplanation(animal)) 
                            ),                            
                            
                            ans.value match {
                                case AnswerValue.Numeric(v) => 
                                    likertScale("q" + q.num, AnswerValue.Numeric(v)) { v => answers.value = answers.value.updated(q.num, ans.copy(value = v)) }
                                    //stronglyAgreeSlider(AnswerValue.Numeric(v)) { v => answers.value = answers.value.updated(q.num, ans.copy(value = v)) }
                                case AnswerValue.Rated(v) => 
                                    ratingPicker(AnswerValue.Rated(v)) { v => answers.value = answers.value.updated(q.num, ans.copy(value = v)) }
                            },                            

                            <.div(^.style := "text-align: center", {
                                footerSelectors(q.num).value match {
                                    case FooterSelection.None => Seq()
                                    case FooterSelection.Confidence => 
                                        confidenceSlider(ans.confidence) { c => answers.value = answers.value.updated(q.num, ans.copy(confidence = c)) }
                                    case FooterSelection.Explanation => 
                                        <.div(^.style := "margin: 1em; text-align: left;", 
                                            q.shortExplanation(animal)
                                            // "Help and advice on answering the question will be added here but is still being worked on..."
                                        )
                                    case FooterSelection.Notes => 
                                        <.div(^.style := "margin: 1em; text-align: left;", 
                                          <.h4("Notes"),
                                          <.textarea(^.style := "width: 100%;",
                                            ^.prop.value := ans.note.getOrElse(""),
                                            ^.onChange ==> { (e) => for v <- e.inputValue do answers.value = answers.value.updated(q.num, ans.copy(note = Some(v))) }
                                          )                                        
                                        )
                                    case FooterSelection.Photo => 
                                        <.div(^.style := "margin: 1em; text-align: left;", 
                                          "Photo feature to be added..."
                                        )
                                }

                            }),

                            // Mid-space controls

                            <.p(^.style := "margin: 1em;", " "),

                            // Footer controls
                            <.div(^.cls := questionFooterStyle,
                                confidenceButton(answers.value(q.num).confidence, footerSelectors(q.num)),
                                footerButton(footerSelectors(q.num), "?", FooterSelection.Explanation),
                                footerButton(footerSelectors(q.num), <.span(^.cls := "material-symbols-outlined", "edit_note"), FooterSelection.Notes),
                               // footerButton(footerSelectors(q.num), <.span(^.cls := "material-symbols-outlined", "photo_camera"), FooterSelection.Photo),

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

/** A version of the assessment form that is spread over a few pages */
case class PagedAssessmentForm(animal:Animal) extends DHtmlComponent {
    import html.{<, ^}


    enum Page:
        case SituPage
        case DomainPage(d:Domain)

    val page = stateVariable(Page.SituPage)
    val assessment = stateVariable(Assessment(animal.id, Situation.DayToDay, new scalajs.js.Date().valueOf, Map.empty))

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

    def submit():Unit = 
        DataStore.addAssessment(assessment.value)
        Router.routeTo(AppRoute.Animal(animal.id))


    def situationPage = 
      <.div(
        leftBlockHeader(
            Router.path(AppRoute.Front),
            "Assessment",
            <.label(^.cls := (animalName), animal.name)
        ),

        <.p(^.style := "margin: 1em;",
            s"Which situation is this assessment for?",            
        ),

        <.p(^.style := "margin: 1em;",
            <.select(^.style := s"margin-left: 0.25em; max-width: 300px; font-size: $largeFont;",
                    ^.on("input") ==> { e => for n <- e.inputValue do assessment.value = assessment.value.copy(situation = Situation.fromOrdinal(n.toInt)) },

                    for s <- allowableSituations(animal.species) yield 
                        <.option(
                            ^.prop.value := s.ordinal, s.toString,
                            if s.ordinal == assessment.value.situation.ordinal then ^.prop.selected := "selected" else None
                        )
                )
        ),

        <.p(^.style := "margin: 1em;",
            s"Thinking of ${animal.name}, what is your level of agreement with each of the following statements",
        ),

        <.p(^.style := "margin: 1em;",
            <.button(^.cls := (button), "Begin", ^.onClick --> { page.value = Page.DomainPage(Domain.values(0)) })
        )

        // TODO: Next page


      )

    def domainPage(domain:Domain) =

        val dqs = domainQuestions(domain)
        val maxQNum = dqs.map(_.num).max

        <.div(
            leftBlockHeader(
                Router.path(AppRoute.Front),
                "Assessment",
                <.label(^.cls := (animalName), animal.name)
            ),

            scoringInProgress(assessment.value, domain),

            <.div(^.cls := (surveyQstyle),
                <.div(^.style := s"padding: 5px 1em; background: ${domain.color}",                    
                    <.label(^.style := "color: white", domainLogo(domain), domain.title),
                ),

                <.div(             
                    for q <- domainQuestions(domain) yield
                        assessment.value.answers.get(q.num) match {
                            case None => 
                                <.div()

                            case Some(ans) =>
                                <.div(^.style := s"border-bottom: 1px solid ${domain.color}", ^.attr("id") := s"question${q.num}",
                                    <.div(^.style := "margin: 1em;",
                                        <.h4(q.headline(animal)),
                                        // <.p(q.shortExplanation(animal)) 
                                    ),                            
                                    
                                    ans.value match {
                                        case AnswerValue.Numeric(v) => 
                                            <.span() // stronglyAgreeSlider(AnswerValue.Numeric(v)) { v => answers.value = answers.value.updated(q.num, ans.copy(value = v)) }
                                        case AnswerValue.Rated(v) => 
                                            <.span()// ratingPicker(AnswerValue.Rated(v)) { v => answers.value = answers.value.updated(q.num, ans.copy(value = v)) }
                                    },                            

                                    <.div(^.style := "text-align: center", {
                                        footerSelectors(q.num).value match {
                                            case FooterSelection.None => Seq()
                                            case FooterSelection.Confidence => 
                                                confidenceSlider(ans.confidence) { c => assessment.value = assessment.value.copy(answers = assessment.value.answers.updated(q.num, ans.copy(confidence = c))) }
                                            case FooterSelection.Explanation => 
                                                <.div(^.style := "margin: 1em; text-align: left;", 
                                                    q.shortExplanation(animal)
                                                    // "Help and advice on answering the question will be added here but is still being worked on..."
                                                )
                                            case FooterSelection.Notes => 
                                                <.div(^.style := "margin: 1em; text-align: left;", 
                                                <.h4("Notes"),
                                                <.textarea(^.style := "width: 100%;",
                                                    ^.prop.value := ans.note.getOrElse(""),
                                                    ^.onChange ==> { (e) => for v <- e.inputValue do assessment.value = assessment.value.copy(answers = assessment.value.answers.updated(q.num, ans.copy(note = Some(v)))) }
                                                )                                        
                                                )
                                            case FooterSelection.Photo => 
                                                <.div(^.style := "margin: 1em; text-align: left;", 
                                                "Photo feature to be added..."
                                                )
                                        }

                                    }),

                                    // Mid-space controls

                                    <.p(^.style := "margin: 1em;", " "),

                                    // Footer controls
                                    <.div(^.cls := questionFooterStyle,
                                        confidenceButton(assessment.value.answers(q.num).confidence, footerSelectors(q.num)),
                                        footerButton(footerSelectors(q.num), "?", FooterSelection.Explanation),
                                        footerButton(footerSelectors(q.num), <.span(^.cls := "material-symbols-outlined", "edit_note"), FooterSelection.Notes),
                                    // footerButton(footerSelectors(q.num), <.span(^.cls := "material-symbols-outlined", "photo_camera"), FooterSelection.Photo),

                                        if q.num < maxQNum then <.div(^.style := "text-align: right; margin: 1em;",
                                            <.button(^.cls := (button), "Next ↓", ^.onClick --> scrollQIntoView(q.num + 1))
                                        ) else Seq()                 
                                    )
                                    
                                )

                        }


                    )

            ),

            <.div(^.style := "text-align: right; margin: 1em;",
                <.button(^.cls := (button, primary), "Submit", ^.onClick --> submit())
            )
        )



    def render = 
        page.value match {
            case Page.SituPage => situationPage
            case Page.DomainPage(d) => domainPage(d) 
        }
        
    
}


/**
 * The assessment page for a given animal
 */
def assessmentPage(aId: AnimalId) = 
    val a = DataStore.animal(aId)
    assessments.AssessmentForm(a)
