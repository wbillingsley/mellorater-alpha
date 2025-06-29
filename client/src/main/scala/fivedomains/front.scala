package fivedomains

import com.wbillingsley.veautiful.*
import html.{Styling, VHtmlContent, DHtmlComponent}
import typings.std.stdStrings.mark

object AnimalList extends DHtmlComponent {
    import html.{<, ^}

    enum OrderBy:
        case LeastRecentlyUpdated
        case Alphabetical

    val listMode = stateVariable(OrderBy.LeastRecentlyUpdated)

    def sortedAnimals = listMode.value match {
        case OrderBy.Alphabetical => 
            DataStore.animals.sortBy(_.name)
        case OrderBy.LeastRecentlyUpdated => 
            DataStore.animals.sortBy { a => DataStore.assessments.filter(_.animal == a.id).sortBy(_.time).map(_.time).lastOption.getOrElse(0d) }

    }

    def switcher = <.div(        

        ^.cls := Styling(
            "text-align: center; margin: 1.5em;"
        ).modifiedBy().register().className,


        listMode.value match {
            case OrderBy.Alphabetical => 
                Seq(
                    <.button(^.cls := (button, "active"), ^.attr("disabled") := "disabled", "Alphabetical"),
                    <.button(^.cls := (button, "enabled"), "Survey due", ^.onClick --> listMode.receive(OrderBy.LeastRecentlyUpdated)),
                    <.a(^.href := Router.path(AppRoute.Settings), <.span(^.cls := "material-symbols-outlined", "settings"   , ^.style := "float: right; right: 20px;"))
                )
            case OrderBy.LeastRecentlyUpdated => 
                Seq(
                    <.button(^.cls := (button, "enabled"), "Alphabetical", ^.onClick --> listMode.receive(OrderBy.Alphabetical)),
                    <.button(^.cls := (button, "active"), ^.attr("disabled") := "disabled", "Survey due"),
                    <.a(^.href := Router.path(AppRoute.Settings), <.span(^.cls := "material-symbols-outlined", "settings", ^.style := "float: right; right: 20px;"))
                )
        }
        
    )

    def render = 
        val hasReal = DataStore.hasRealData
        val hasTest = DataStore.hasTestData

        <.div(^.style := "margin: 1em;",
            Seq(
                switcher,
                (if hasReal then 
                    <.div(
                        for a <- sortedAnimals.filter(!_.testData) yield animals.summaryCard(a)
                    )
                else 
                    if hasTest then
                        <.div(^.cls := (notice),
                            <.h3("It's time to add your first animal"),
                            <.p("It looks like you haven't added any real animals yet. Click the button below to add your first animal.")
                        )                         
                    else 
                        <.div(^.cls := (notice),    
                            <.h3("It's time to add your first animal"),
                            <.p("This page will show the summary cards for your animals, but it looks ike you haven't added any animals yet. Click the button below to add your first animal")
                        )
                        
                )
            ),

            <.p(^.style := "margin-top: 1em; text-align: center;",
                <.a(^.cls := (button, primary), ^.href := Router.path(AppRoute.AddAnimal), "Add an animal")
            ),

            (if hasTest then <.div(
                <.div(^.cls := (notice),
                    <.h4("Demo animals:"),
                    <.p("These are demo animals so you can see what an assessed animal looks like. You can remove these demo animals from the settings screen."),                
                ),
                <.div(
                    for a <- sortedAnimals.filter(_.testData) yield animals.summaryCard(a)
                )
            ) else 
                <.span()
            ),

            if !hasReal && !hasTest then 
                <.p(^.cls := (notice), "Hint: If you want to add some demo animals, just to see what assessed animals look like, you can add some from the Settings screen.")
            else None
        )

}

case class SensitiveTopicNotice() extends DHtmlComponent {
    import html.{<, ^}

    val save = stateVariable(true)

    def render = <.div(^.cls := (notice),

        markdown.div("""
        |## Before You Begin
        |
        |This app helps you assess your animal's welfare using the Five Domains Model: nutrition, environment, health, behaviour, and mental state.
        |
        |At times, your assessment may highlight signs of welfare compromise as well as improvement. Some users may find it distressing to recognise signs of decline.
        |
        |Participation is voluntary. By continuing, you are telling us that you have read the information provided and that you voluntarily agree to take part in the research. You will also be asked to confirm whether or not you consent to your anonymous responses being used for research aimed at improving animal welfare tools. No personal information is collected.
        |
        |You can stop at any time. You can also withdraw your consent to participate in the research or to send data via the app's settings.
        |
        |For more information, tap [More Info] to read the full Participant Information Sheet and Consent Form.
        |
        |""".stripMargin),
        <.div(^.style := "text-align: right;",
            <.input(^.attr("id") := "dont-show-senstop-again", ^.attr.`type` := "checkbox", ^.prop.checked := save.value, ^.onChange --> { save.value = !save.value }), 
            <.label(^.attr("for") := "dont-show-senstop-again", "Don't show this again "),
            <.button(^.cls := (button, noticeButton), "Continue", ^.onClick --> { DataStore.acceptSensitiveTopics(save.value) }),
            <.a(^.cls := (button, infoButton), "More Info", ^.href := "https://www.mellorater.org/research-consent")
        )
    )

}

def frontPage = html.<.div(
    frontHeader,
    if DataStore.acceptedSensitiveTopics.value then AnimalList else SensitiveTopicNotice(),
)
