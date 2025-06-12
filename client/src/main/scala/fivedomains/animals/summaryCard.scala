package fivedomains.animals

import com.wbillingsley.veautiful.*
import html.{Styling, VHtmlContent, DHtmlComponent, <, ^, EventMethods}

import fivedomains.{given, *}
import model.*
import fivedomains.assessments.*

def summaryCard(animal:Animal) = 
    val surveys = DataStore.surveysFor(animal).sortBy(_.time)

     <.div(
        ^.cls := (card, backgrounds(animal.display)),
        <.div(
            <.a(^.cls := "title", ^.href := Router.path(AppRoute.Animal(animal.id)), 
                <.label(^.cls := (animalName), animal.displayName)
            ),

            <.div(^.attr.style := "float: right;",
                animal.species.longText
        
            )
        ),
       
        <.div(^.attr.style := "margin: 1em 0;", scoringRose(surveys.reverse)),

         <.div(
            if surveys.isEmpty then 
                <.div("Never assessed") else 
                <.div(
                    <.div(s"${surveys.length} assessments recorded"),
                    {
                        val dt = (new scalajs.js.Date().valueOf - surveys.last.time) / (1000 * 60 * 60 * 24)
                        <.div(s"Last assessed ${dt.toInt} days ago")
                    }
                )
        ),

        <.div(^.style := "text-align: right;",
            <.button(^.cls := (button, primary), "Assess", ^.onClick --> Router.routeTo(AppRoute.Assess(animal.id)))
        )            
     )


def emptyCard = <.div(^.cls := (
        Styling("""|border: 1px dashed #aaa;
                   |border-radius: 0.25em;
                   |text-align: center;
                   |""".stripMargin
        ).modifiedBy(
            " .title" -> 
                """|-webkit-text-stroke-width: 1px;
                   |-webkit-text-stroke-color: #aaa;
                   |""".stripMargin
        ).register(),
        card
    ),
    <.h3(^.cls := "title", "You haven't registered any animals"),
)

def animalCard(a:Animal) = <.div(
    ^.cls := (
        card
    ),


)
