package fivedomains

import com.wbillingsley.veautiful.*
import html.{VHtmlContent, Styling, DHtmlComponent, Animator}
import fivedomains.testdata.addPickles
import fivedomains.DataStore.hasTestData

object SettingsScreen extends DHtmlComponent {
    import html.{<, ^}

    def render = <.div(
        "Settings"
    )

}

/** A widget for resetting the first time notice */
case class ResetFirstTimeNotice() extends DHtmlComponent {

    val enabled = stateVariable(false)

    def clearFlag() = 
        DataStore.clearAcceptSensitiveTopics() 

    override def render = {
        import html.{<, ^}
        <.div(
            <.p(
                """|The first use notice shows a sensitive topics warning and other disclaimers. This resets the flag about whether it should appear
                   |""".stripMargin

            ),

            <.input(^.attr("type") := "checkbox", ^.prop.checked := enabled.value, ^.onChange --> { enabled.value = !enabled.value}),
            <.label("Tick to unlock"),
            <.button(^.cls := (button, noticeButton), ^.prop.disabled := !enabled.value, "Reset first use notice", ^.onClick --> clearFlag())
        )
    }

}


/** A widget for resetting the first time notice */
case class ResetEventLog() extends DHtmlComponent {

    val enabled = stateVariable(false)

    def clearLogs() = 
        Analytics.resetLogs()
        enabled.value = false

    override def render = {
        import html.{<, ^}
        <.div(
            <.p(
                """|The app keeps a local event log in your browser for analytics. (This is not sent to any server anywhere.) This can be cleared.
                   |After clearing, I recommend refreshing the page, in order to record a new start-of-session event.
                   |""".stripMargin

            ),

            <.input(^.attr("type") := "checkbox", ^.prop.checked := enabled.value, ^.onChange --> { enabled.value = !enabled.value}),
            <.label("Tick to unlock"),
            <.button(^.cls := (button, noticeButton), ^.prop.disabled := !enabled.value, "Reset event log", ^.onClick --> clearLogs())
        )
    }

}

/** A widget for resetting all locally saved data ready for the next user */
case class ResetData() extends DHtmlComponent {

    val enabled = stateVariable(false)

    def reset() = 
        DataStore.clearAll()
        enabled.value = false

    override def render = {
        import html.{<, ^}
        <.div(
            <.p(
                """|The app currently stores its data only in the browser's local storage. 
                   |If the data is reset, it cannot be recovered.
                   |""".stripMargin

            ),

            <.input(^.attr("type") := "checkbox", ^.prop.checked := enabled.value, ^.onChange --> { enabled.value = !enabled.value}),
            <.label("Tick to unlock"),
            <.button(^.cls := (button, dangerButton), ^.prop.disabled := !enabled.value, "Reset all data", ^.onClick --> reset())
        )
    }

}


/** A widget for adding or removing demo animals */
case class DemoData() extends DHtmlComponent {

    def clear() = 
        DataStore.clearDemoAnimals()
        enabled.value = false

    def add() = 
        addPickles()
        enabled.value = false

    val enabled = stateVariable(false)


    override def render = {
        import html.{<, ^}

            if hasTestData then 
                <.div(
                        <.p(
                        """|To remove the demo animals, click the button below. You can re-add the demo animals again afterwards, but any assessments
                           |you've made of them will be lost.
                        |""".stripMargin
                        ),

                        <.input(^.attr("type") := "checkbox", ^.prop.checked := enabled.value, ^.onChange --> { enabled.value = !enabled.value}),
                        <.label("Tick to unlock"),
                        <.button(^.cls := (button, noticeButton), ^.prop.disabled := !enabled.value, "Clear demo animals", ^.onClick --> clear())
                )

            else
                <.div(
                        <.p(
                        """|The app can add some demo animals for you. It marks them as not being real animals, so they're easy to remove later.
                        |""".stripMargin
                        ),

                        <.input(^.attr("type") := "checkbox", ^.prop.checked := enabled.value, ^.onChange --> { enabled.value = !enabled.value}),
                        <.label("Tick to unlock"),
                        <.button(^.cls := (button, noticeButton), ^.prop.disabled := !enabled.value, "Add demo anials", ^.onClick --> add())
                )
    }

}

def settingsPage = 
    import html.* 
    <.div(
        leftBlockHeader(
            Router.path(AppRoute.Front),
            "Settings",
            <.p()
        ),

        <.div(^.style := "margin: 1em;",


        <.h2("First use"),
        ResetFirstTimeNotice(),

        <.h2("Logs"),
        ResetEventLog(),

        <.h2("Data"),
        ResetData(),
        DemoData()

        
        )

    )
