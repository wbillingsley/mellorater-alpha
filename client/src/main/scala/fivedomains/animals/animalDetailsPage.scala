package fivedomains.animals

import fivedomains.{given, *}
import model.*
import com.wbillingsley.veautiful.*
import html.*
import assessments.*


/**
 * A short summary message indicating how many assessments we've got on file, etc
 */
def assessmentQuantumStats(a:Animal, surveys:Seq[Assessment]) =
    // TODO - check if many of the surveys have been with low confidence
    <.div(^.cls := nakedParaMargins,
        <.p(
            f"Overall confidence across the surveys was ${surveys.overallConfidence * 100}%.0f%%."
        ),
        <.p(
            for s <- surveys.lastOption.toSeq yield <.div(
                s"The most revent survey was on ${new scalajs.js.Date(s.time).toLocaleDateString}. ",
                f"Overall confidence in this survey was ${s.overallConfidence * 100}%.0f%%.",
            )

        )
        
    ) 


def currentRedFlags(assess:Assessment) = 
    val animal = DataStore.animalMap(assess.animal)
    <.div(^.cls := nakedParaMargins,
        <.h3("Current concerns"),

        if assess.domainsContainingConcern.isEmpty then <.p("None") else None,

        // For each domain, if there exists a value scored low, show the section
        for d <- assess.domainsContainingConcern yield <.div(
            <.h4(d.title),
            for a <- assess.lowAnswers if a.question.domain == d yield <.div(
                <.h5(
                    <.div(^.style := "float: left; margin-right: 10px; margin-top: 2px;", boxedScoreFaceHtml(Some(a.value.asDouble))), 
                    a.question.headline(animal)
                ),
                
                for n <- a.note yield <.p(^.style := "font-style: italic;", n)
            )
        )        

    )


def pastRedFlags(assessments:Seq[Assessment], mode:AnswerFilter):DHtmlModifier = 
    if assessments.isEmpty then 
        None
    else
        val animal = DataStore.animalMap(assessments.head.animal)

        <.div(
            for assess <- assessments.sortBy(- _.time) yield 

                val filteredAnswers = assess.answers.toSeq.sortBy(_._1).filter((num, a) => 
                    mode match {
                        case AnswerFilter.Everything => !a.question.dontAsk
                        case AnswerFilter.LowConfidence => !a.question.dontAsk && a.confidence.low
                        case AnswerFilter.LowScore => !a.question.dontAsk && Seq(Agreement.Neutral, Agreement.Disagree, Agreement.StronglyDisagree).contains(a.value.agreement)
                        case AnswerFilter.HasNote => !a.question.dontAsk && a.note.nonEmpty
                    }
                    
                )


                <.div(^.cls := nakedParaMargins,

                    // <.h3(new scalajs.js.Date(assess.time).toLocaleDateString),

                    <.table(

                        if filteredAnswers.isEmpty then 
                            <.p("No questions matched the filter")
                        else
                            for (i, a) <- filteredAnswers yield 
                                <.tr(^.style := "vertical-align: top",
                                    <.td(
                                        unboxedDomainLogo(a.question.domain, assess.categoryScore(a.question.domain))
                                    ),
                                    <.td(
                                        boxedScoreFaceHtml(Some(a.value.asDouble))
                                    ),
                                    <.td(
                                        if a.confidence.low then <.span(^.cls := "material-symbols-outlined", "question_mark") else " "
                                    ),
                                    <.td(
                                        <.h5(a.question.headline(animal), ^.style := "margin-top: 0"),

                                        for n <- a.note yield <.p(^.style := "font-style: italic;", n),

                                        <.div(^.style := "font-variant: all-small-caps",
                                            feedback(animal, assess, QuestionIdentifier.fromOrdinal(a.q))
                                        )

                                    )
                                )

                    
                    )

                )
        )

def animalDetailsPage(aId:AnimalId) = 
    val a = DataStore.animal(aId)
    val surveys = DataStore.surveysFor(a)

    <.div(
        leftBlockHeader(
            Router.path(AppRoute.Front),
            "Animal details",
            <.label(^.cls := (animalName), a.name)
        ),

        SurveySelectWidget(a, surveys)

    )


enum AnswerFilter(val text:String):
    case Everything extends AnswerFilter("Everything")
    case LowConfidence extends AnswerFilter("Low confidence questions")
    case LowScore extends AnswerFilter("Low scoring questions")
    case HasNote extends AnswerFilter("Questions with notes")

case class SurveySelectWidget(animal:Animal, surveys:Seq[Assessment]) extends DHtmlComponent {

    val mode = stateVariable(AnswerFilter.Everything)

    val max = surveys.length
    val number = stateVariable(max)

    def subset = surveys.reverse.drop(max - number.value)

    override def render = <.div(

        <.div(^.cls := (alignCentreStyle, stickyTop, bgWhite),    

            scoringRose(subset),

            subset.headOption match {
                    case Some(assess) => 
                        val d = new scalajs.js.Date(assess.time)
                        <.h4(d.toLocaleDateString(), " ", d.toLocaleTimeString())
                    case None => <.span()
            },

            <.p(
                
                "Time machine: ",
                <.button(^.cls := "button material-symbols-outlined", "arrow_left", ^.prop.disabled := number.value <= 1, ^.onClick --> {number.value = number.value - 1}),
                <.input(
                    ^.attr("type") := "range", ^.attr("min") := 1, ^.attr("max") := max,
                    ^.prop("value") := number.value, ^.on.input ==> { e => for v <- e.inputValue do number.value = v.toInt }
                ),
                <.button(^.cls := "button material-symbols-outlined", "arrow_right", ^.prop.disabled := number.value >= max, ^.onClick --> {number.value = number.value + 1}),
            ),


            <.p(
                "Show ",
                <.select(^.style := s"margin-left: 0.25em; ",
                    ^.on.change ==> { (e) => 
                        val n = e.target.asInstanceOf[scalajs.js.Dynamic].value.asInstanceOf[String]
                        mode.value = AnswerFilter.fromOrdinal(n.toInt) 
                    },
                    for s <- AnswerFilter.values yield 
                        <.option(
                            ^.prop.value := s.ordinal, s.text,
                            if mode.value == s then ^.prop.selected := "selected" else None
                        )
                )
            ),


        ),

        pastRedFlags(subset.headOption.toSeq, mode.value)
    )

}


def surveySummary(animal:Animal, surveys:Seq[Assessment]) = <.div(
    // Widget for surveys
    if surveys.isEmpty then 
        scoringRose(surveys.reverse)
    else 
        <.div(^.style := "margin: 1.5em;",
            <.div(^.style := "text-align: center;",
                scoringRose(surveys.reverse)
            ),

            pastRedFlags(surveys, AnswerFilter.Everything)
        ),

    <.div(^.cls := nakedParaMargins,
        // assessmentQuantumStats(animal, surveys),
        // warningFlags(surveys),
        // lowestRatingAdvice(surveys), 
        // confidenceAdvice(surveys)   
    )
)

def warningFlags(surveys:Seq[Assessment]) = <.div(
    <.p("Spot for warning flags we detect from these assessments...")
)

def lowestRatingAdvice(surveys:Seq[Assessment]) = <.div(
    <.p("Spot for highlighting the trend of questions that have been the lowest rated in recent surveys...")
)

def confidenceAdvice(surveys:Seq[Assessment]) = <.div(
    <.p("Spot for highlighting issues in confidence in recent questions...")
)