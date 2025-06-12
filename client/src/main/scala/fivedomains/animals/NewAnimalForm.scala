package fivedomains.animals

import com.wbillingsley.veautiful.*
import html.{Styling, VHtmlContent, DHtmlComponent, <, ^, EventMethods}

import fivedomains.{given, *}
import model.*

val formStyling = Styling(
    """|display: flex;
       |flex-direction: row;
       |""".stripMargin
).modifiedBy(
    " label" -> "flex: none; width: 75px;",
    " input,select" ->"margin-bottom: 10px; margin-top: -4px;"


).register()

class AnimalForm() extends DHtmlComponent {

    val animal = stateVariable(Animal(DataStore.nextAnimalId, "", Species.Horse))

    def add():Unit = 
        DataStore.addAnimal(animal.value)
        Router.routeTo(AppRoute.Front)

    def render = <.div(
        leftBlockHeader(
            Router.path(AppRoute.Front),
            "Add an animal",
            <.label(^.cls := (animalName), if animal.value.name.nonEmpty then animal.value.name else "New Animal")
        ),

        emptyAnimalsNotice,

        <.div(
            ^.cls := (card, backgrounds(animal.value.display)),
            <.div(^.cls := formStyling,
                <.label("Name"),
                <.input(^.style := s"margin-left: 0.25em; font-size: $largeFont;",
                    ^.prop("value") := animal.value.name, ^.prop.placeholder := "Animal name",
                    ^.on("input") ==> { e => for n <- e.inputValue do animal.value = animal.value.copy(name = n) }
                )
            ),
            <.div(^.cls := formStyling,
                <.label("Species"),
                <.select(^.style := s"margin-left: 0.25em; max-width: 300px; font-size: $largeFont;",
                    ^.on.change ==> { (e) => 
                        val n = e.target.asInstanceOf[scalajs.js.Dynamic].value.asInstanceOf[String]
                        animal.value = animal.value.copy(species = Species.values(n.toInt)) 
                    },
                    for s <- Species.values yield 
                        <.option(
                            ^.prop.value := s.ordinal, s.longText,
                            if s.longText == animal.value.species.longText then ^.prop.selected := "selected" else None
                        )
                )
            ),
            <.div(^.cls := formStyling,
                <.label("Breed"),
                <.input(^.style := s"margin-left: 0.25em; font-size: $largeFont;",
                    ^.prop("value") := animal.value.breed, ^.attr.placeholder := "Unspecified",
                    ^.on("input") ==> { e => for n <- e.inputValue do animal.value = animal.value.copy(breed = n) }
                )
            ),

            <.div(^.cls := formStyling,
                <.label("Sex"),
                <.select(^.style := s"margin-left: 0.25em; max-width: 300px; font-size: $largeFont;",
                    ^.on.change ==> { (e) => 
                        val n = e.target.asInstanceOf[scalajs.js.Dynamic].value.asInstanceOf[String]
                        animal.value = animal.value.copy(sex = Sex.values(n.toInt)) 
                    },
                    for s <- Sex.values yield 
                        <.option(
                            ^.prop.value := s.ordinal, s.prettyString,
                            if s == animal.value.sex then ^.prop.selected := "selected" else None
                        )
                )
            ),


            <.div(^.style := "text-align: right;",
                <.button(^.cls := (button), "📷"),
                <.button(^.cls := (button, noticeButton), "Add", ^.onClick --> add())
            )
        )
    )
}


def emptyAnimalsNotice = if DataStore.hasRealData then <.div() else <.div(^.cls := (notice),
    markdown.div(
        """|### Let's add your first animal
           |
           |Mostly, the app just needs to know your animal's name and species.
           |However, you can also pick a card background to help you tell them apart at a glance
           |until we've added a photo feature.
           |""".stripMargin
    )
)

def addAnimalPage = <.div(
    animals.AnimalForm()
)