package fivedomains.assessments

import com.wbillingsley.veautiful.html
import fivedomains.model.*

def feedback(animal:Animal, asst:Assessment, q:QuestionIdentifier): String | html.VHtmlContent = {

    val ans = asst.answers(q.ordinal)

    def lowConfidence(q:QuestionIdentifier) = asst.answers(q.ordinal).confidence.low
    def stronglyAgree(q:QuestionIdentifier) = asst.answers(q.ordinal).value

    import html.{<, ^}

    q match {
        case _ if ans.value.agreement == Agreement.StronglyDisagree && !lowConfidence(q) => 
            <.p(s"Take action! Make the changes needed to allow you to improve this for ${animal.name}.")
        case _ if ans.value.agreement == Agreement.Disagree && !lowConfidence(q) => 
            <.p(s"This is of concern, make the changes needed to improve this for ${animal.name}, and seek advice if you need to.")
        case _ if ans.value.agreement == Agreement.Neutral && !lowConfidence(q) => 
            <.p(s"This is helpful but for ${animal.name}'s sake you need to improve this.")
        case _ if ans.value.agreement == Agreement.Agree && !lowConfidence(q) => 
            <.p(s"Looking good, can you make the changes needed for you to improve this further?")
        case _ if ans.value.agreement == Agreement.StronglyAgree && !lowConfidence(q) => 
            <.p(s"Great job keep it up!")

        case _ if ans.value.agreement == Agreement.StronglyDisagree => 
            <.p(s"Prepare to take action, but double-check your evidence or seek advice on the changes needed for you to improve this.")
        case _ if ans.value.agreement == Agreement.Disagree => 
            <.p(s"This is of concern. Consider what evidence or advice you need to improve this for ${animal.name}.")
        case _ if ans.value.agreement == Agreement.Neutral => 
            <.p(s"This is not helpful. You will need to collect more evidence and make changes to improve this. If you are unsure, ask for advice.")
        case _ if ans.value.agreement == Agreement.Agree => 
            <.p(s"Looking good but consider what more evidence would increase your confidence in this assessment.")
        case _ if ans.value.agreement == Agreement.StronglyAgree => 
            <.p(s"Do you need to review the evidence to improve your confidence in this assessment?")   

        case _ => <.p(s"Oops, a small bug in the app: None of the feedback message options matched the values for this question.")         
    }

    // q match {
    //     case QuestionIdentifier.DrinksEnough if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p("Take action! Make the changes needed to allow you to move to strongly agree")
    //     case QuestionIdentifier.DrinksEnough if !lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, make the changes needed to allow you to strongly agree")
    //     case QuestionIdentifier.DrinksEnough if !lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is helpful but for ${animal.name}'s sake you need to aim for strongly agree")
    //     case QuestionIdentifier.DrinksEnough if !lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good, can you make the changes needed for you to strongly agree?")
    //     case QuestionIdentifier.DrinksEnough if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Great job, keep it up!")

    //     case QuestionIdentifier.DrinksEnough if lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p("Prepare to take action, but double-check your evidence or seek advice on the changes needed for you to strongly agree")
    //     case QuestionIdentifier.DrinksEnough if lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, consider what evidence or advice you need to make changes that will allow you to strongly agree ")
    //     case QuestionIdentifier.DrinksEnough if lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is not helpful, you will need to collect more evidence, if you are unsure ask for advice")
    //     case QuestionIdentifier.DrinksEnough if lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good but consider what more evidence you need to be confident deciding on this statement. ")
    //     case QuestionIdentifier.DrinksEnough if lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Do you need to review your confidence in the evidence? ")            


    //     case QuestionIdentifier.EatsEnough if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p("Take action! Make the changes needed to allow you to move to strongly agree")
    //     case QuestionIdentifier.EatsEnough if !lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, make the changes needed to allow you to strongly agree")
    //     case QuestionIdentifier.EatsEnough if !lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is helpful but for ${animal.name}'s sake you need to aim for strongly agree")
    //     case QuestionIdentifier.EatsEnough if !lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good, can you make the changes needed for you to strongly agree?")
    //     case QuestionIdentifier.EatsEnough if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Great job, keep it up!")

    //     case QuestionIdentifier.EatsEnough if lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p("Prepare to take action, but double-check your evidence or seek advice on the changes needed for you to strongly agree")
    //     case QuestionIdentifier.EatsEnough if lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, consider what evidence or advice you need to make changes that will allow you to strongly agree ")
    //     case QuestionIdentifier.EatsEnough if lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is not helpful, you will need to collect more evidence, if you are unsure ask for advice")
    //     case QuestionIdentifier.EatsEnough if lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good but consider what more evidence you need to be confident deciding on this statement. ")
    //     case QuestionIdentifier.EatsEnough if lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Do you need to review your confidence in the evidence? ")            


    //     case QuestionIdentifier.FoodVariety if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p(s"${animal.name} will benefit from food being presented in ways that are interesting for their species - check with your vet for advice")
    //     case QuestionIdentifier.FoodVariety if !lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, make the changes needed to allow you to strongly agree")
    //     case QuestionIdentifier.FoodVariety if !lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is helpful but for ${animal.name}'s sake you need to aim for strongly agree")
    //     case QuestionIdentifier.FoodVariety if !lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good, can you make the changes needed for you to strongly agree?")
    //     case QuestionIdentifier.FoodVariety if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Great job, keep it up!")

    //     case QuestionIdentifier.FoodVariety if lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p(s"${animal.name} will benefit from food being presented in ways that are interesting for their species - collect more evidence and check with your vet for advice")
    //     case QuestionIdentifier.FoodVariety if lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, consider what evidence or advice you need to make changes that will allow you to strongly agree ")
    //     case QuestionIdentifier.FoodVariety if lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is not helpful, you will need to collect more evidence, if you are unsure ask for advice")
    //     case QuestionIdentifier.FoodVariety if lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good but consider what more evidence you need to be confident deciding on this statement. ")
    //     case QuestionIdentifier.FoodVariety if lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Do you need to review your confidence in the evidence? ")                        


    //     // Low feedback for other categories

    //     case QuestionIdentifier.Lighting if !lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} will benefit from less unpleasant lighting levels, noises and odours. Take appropriate action as soon as possible ")
    //     case QuestionIdentifier.Lighting if lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"Prepare to take action to reduce ${animal.name}'s exposure to unpleasant lighting levels, noises and odours. Collect more evidence and/or seek advice ")

    //     case QuestionIdentifier.ThermalComfort if !lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"Provide access to areas that are clean and promote physical and thermal comfort")
    //     case QuestionIdentifier.ThermalComfort if lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} will benefit from areas that are clean and promote physical and thermal comfort. Take appropriate action as soon as possible")

    //     case QuestionIdentifier.Sleep if !lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"Provide access to areas that are clean and promote physical and thermal comfort")
    //     case QuestionIdentifier.Sleep if lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} will benefit from areas that are clean and promote physical and thermal comfort. Take appropriate action as soon as possible")


    //     case QuestionIdentifier.InjuriesDisease if !lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} should be under veterinary care. If you haven't already, action this.")
    //     case QuestionIdentifier.InjuriesDisease if lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"Seek veterinary advice to check on ${animal.name}'s health status")

    //     case QuestionIdentifier.Bodyweight if !lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} will benefit from continuous monitoring of both health and fitness.")
    //     case QuestionIdentifier.Bodyweight if lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} will benefit from continuous monitoring of both health and fitness. Seek veterinary advice to select appropriate monitoring tools")


    //     case QuestionIdentifier.Pain if !lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} should be under veterinary care. If you haven't already, action this.")
    //     case QuestionIdentifier.Pain if lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} will benefit from continuous monitoring of both pain and distress. Seek veterinary advice to select appropriate monitoring tools")


    //     case QuestionIdentifier.StimulatingEnvironment if !lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} will benefit from a more interesting and stimulating environment. Find out what environmental enrichment is appropriate for ${animal.species.plural}", 
    //             <.a(^.href := "https://www.ufaw.org.uk/why-ufaws-work-is-important/environmental-enrichment-3", "https://www.ufaw.org.uk/why-ufaws-work-is-important/environmental-enrichment-3", ^.attr.target := "_blank")
    //         )
    //     case QuestionIdentifier.StimulatingEnvironment if lowConfidence(q) && Seq(Agreement.Disagree, Agreement.StronglyDisagree).contains(ans.value.agreement) => 
    //         <.p(s"${animal.name} may benefit from a more interesting and stimulating environment. Find out what environmental enrichment is appropriate for ${animal.species.plural}", 
    //             <.a(^.href := "https://www.ufaw.org.uk/why-ufaws-work-is-important/environmental-enrichment-3", "https://www.ufaw.org.uk/why-ufaws-work-is-important/environmental-enrichment-3", ^.attr.target := "_blank")
    //         )


    //     // For the remaining spreadsheet questions, we only have feedback for the low cases, so I'm defaulting the remainder to the boilerplate above

    //     case _ if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p("Take action! Make the changes needed to allow you to move to strongly agree")
    //     case _ if !lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, make the changes needed to allow you to strongly agree")
    //     case _ if !lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is helpful but for ${animal.name}'s sake you need to aim for strongly agree")
    //     case _ if !lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good, can you make the changes needed for you to strongly agree?")
    //     case _ if !lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Great job, keep it up!")

    //     case _ if lowConfidence(q) && ans.value.agreement == Agreement.StronglyDisagree => 
    //         <.p("Prepare to take action, but double-check your evidence or seek advice on the changes needed for you to strongly agree")
    //     case _ if lowConfidence(q) && ans.value.agreement == Agreement.Disagree => 
    //         <.p("This is of concern, consider what evidence or advice you need to make changes that will allow you to strongly agree ")
    //     case _ if lowConfidence(q) && ans.value.agreement == Agreement.Neutral => 
    //         <.p(s"This is not helpful, you will need to collect more evidence, if you are unsure ask for advice")
    //     case _ if lowConfidence(q) && ans.value.agreement == Agreement.Agree => 
    //         <.p(s"Looking good but consider what more evidence you need to be confident deciding on this statement. ")
    //     case _ if lowConfidence(q) && ans.value.agreement == Agreement.StronglyAgree => 
    //         <.p(s"Do you need to review your confidence in the evidence? ")                        


    // }



}