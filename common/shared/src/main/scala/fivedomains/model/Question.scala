package fivedomains.model

import fivedomains.*

case class Question(num:Int, domain:Domain, 
    headline:Animal => String, 
    shortExplanation:Animal => String, 
    longExplanation:Animal => String, 
    kind:QuestionType = QuestionType.StronglyAgreeSlider,
    dontAsk:Boolean = false
) {

    /**
     * The default (unchanged) answer to show for this question when a form is first presented
     */
    def defaultAnswer:Answer = kind match {
        case QuestionType.StronglyAgreeSlider => Answer(num,AnswerValue.Numeric(50), Confidence.Medium)
        case QuestionType.RatingPicker => Answer(num, AnswerValue.Rated(Rating.Neutral), Confidence.Medium)
    }

}

type QuestionSection = Domain

enum QuestionType derives upickle.default.ReadWriter:
    case StronglyAgreeSlider
    case RatingPicker


enum QuestionIdentifier:
    case Overall
    case DrinksEnough
    case EatsEnough
    case FoodVariety
    case Lighting
    case ThermalComfort
    case Sleep
    case InjuriesDisease
    case Bodyweight
    case Pain
    case StimulatingEnvironment
    case Room
    case Exercise
    case SameSpeciesInteraction
    case OtherSpeciesInteraction
    case OtherSpeciesConflict
    case CarerBond
    case FamiliarPeople
    case CalmWithPeople

val allQuestions:Seq[(QuestionSection, Seq[Question])] = Seq(
    Domain.Mental -> Seq(
        Question(0, Domain.Mental, { 
            (a:Animal) => s"Overall, how would you rate ${a.name}'s wellbeing?" }, 
            (_) => "From a high level persepective (or 'blurring your vision'), what would you consider the animal's overall well-being to be?",
            (a:Animal) => s"""Over time, the more detailed questions in each category will help the app to understand what domains to focus on for ${a.name}. Keeping track of a human high-level judgment of the animal's wellbeing, however, helps to understand what detailed changes will predict declines and improvements in overall well-being.""",
            QuestionType.RatingPicker, true
        ),
    ),
    Domain.Nutrition -> Seq(
        Question(QuestionIdentifier.DrinksEnough.ordinal, Domain.Nutrition, { 
            (a:Animal) => s"${a.name} drinks enough clean water." },
            (a:Animal) => s"Is ${a.name} well hydrated and able to access and drink enough clean water whenever they are thirsty?",
            (_) => """|From a welfare assessment perspective, the welfare compromise occurs when the animal is not able to resolve their need to drink. A thirsty animal deprived from water will experience frustration. 
                      |""".stripMargin
        ),
        Question(QuestionIdentifier.EatsEnough.ordinal, Domain.Nutrition, { 
            (a:Animal) => s"${a.name} eats nutritious, palatable, and sufficient food." },
            (a:Animal) => s"Is ${a.name} able to access enough nutritious and palatable food whenever they are hungry?",
            (_) => """|From a welfare perspective, when we talk about animals receiving sufficient food, or animals being hungry, we are not talking about them being too thin or malnourished. We are referring to them fasting for periods longer than are healthy and natural. 
                      |""".stripMargin
        ),

        Question(QuestionIdentifier.FoodVariety.ordinal, Domain.Nutrition, { 
            (a:Animal) => s"${a.name} is offered food in ways that are varied and interesting for their species." },
            (a:Animal) => s"Is eating a pleasurable experience for ${a.name}? is the way they access their food how they would choose to eat? For example, are they able to forage or spend time searching and finding food?",
            (_) => """|The key to responding to this statement is to assess whether the way the food is presented is more, or less, compatible with the animal’s evolved characteristics.
                      |""".stripMargin            
        ),    
    ),
    Domain.Environment -> Seq(
        Question(QuestionIdentifier.Lighting.ordinal, Domain.Environment, { 
            (a:Animal) => s"${a.name} can avoid unpleasant lighting levels, noises, and odours." },
            (a:Animal) => s"Is ${a.name} able to avoid experiencing events or stimuli that would cause them stress or discomfort? Think about the type of situations or environments that [name] would naturally seek to avoid.",
            (_) => """Just because an animal tolerates a noise, an odour or a certain level of lighting does not mean that they woud rather not experience it."""
        ),
        Question(QuestionIdentifier.ThermalComfort.ordinal, Domain.Environment, { 
            (a:Animal) => s"${a.name} can choose to occupy areas that are clean and promote physical and thermal comfort." },
            (a:Animal) => s"During times of activity, is ${a.name} able to move around and remain comfortable and safe in their environment? Think about the type of places that ${a.name} would naturally choose to occupy.",
            (_) => """The enclosure should be well drained, clean and compliant and allow that animal to move around, stand or lie down on a surface that suits their species"""
        ),
        Question(QuestionIdentifier.Sleep.ordinal, Domain.Environment, { 
            (a:Animal) => s"${a.name} lives in a place that allows them to be calm, to rest, and to sleep." },
            (a:Animal) => s"During times of rest, is ${a.name} able to relax and sleep without being exposed to environmental, physical or psychological stressors that might lead to reduced sleep?",
            (_) => """The animal can sleep where and when it chooses to"""
        ),    
    ),
    Domain.Health -> Seq(
        Question(QuestionIdentifier.InjuriesDisease.ordinal, Domain.Health, { 
            (a:Animal) => s"${a.name} has no injuries, signs of illness or disease" },
            (a:Animal) => s"Is ${a.name} in good health, with no indication of existing disease or injury, based on your general understanding of good and ill-health that could impact ${a.name}'s experience.",
            (_) => """This does not replace either a professional exam nor expert advice in any way. Veterinarians are essential members of your animal’s health support team"""
        ),
        Question(QuestionIdentifier.Bodyweight.ordinal, Domain.Health, { 
            (a:Animal) => s"${a.name} has a healthy bodyweight and has an appropriate level of fitness" },
            (a:Animal) => s"Is ${a.name} in good condition (neither too fat or too thin) and fit enough to be able to exercise and conduct their daily activities without discomfort?",
            (_) => """Although body condition is related to the animal’s nutrition, in the Health Domain, the concern is with its influence on physical and mental (body) function. It is relevant because carrying the right amount of ‘fatness’ is associated with healthy body function, whereas animals who are either too thin or too fat can suffer various health problems."""
        ),
        Question(QuestionIdentifier.Pain.ordinal, Domain.Health, { 
            (a:Animal) => s"${a.name} shows no signs of acute or chronic pain, or distress." },
            (a:Animal) => s"Is ${a.name} able to move, eat, rest and interact normally with no apparent discomfort or pain, such as lameness or stiffness?",
            (_) => """As well as assessing the welfare of your own animals, you should feel comfortable asking a good, knowledgeable friend or colleague to do so. Learning about these discomfort, pain and stress behaviours can help us identify what’s going on for the animal and how they are experiencing the world.You might know the signs of disease and pain, but fail to recognise them in your own animals. This can be especially noticeable when the ailment has a slow onset or is chronic.
Such ‘blind spots’ can happen because we become habituated or desensitised. """"
        ),    
    ),
    Domain.InteractionsEnvironment -> Seq(
        Question(QuestionIdentifier.StimulatingEnvironment.ordinal, Domain.InteractionsEnvironment, { 
            (a:Animal) => s"${a.name} lives in an interesting and stimulating place" },
            (a:Animal) => s"Is ${a.name} regularly using all the available areas of their home environment? Does it contains stimuli relevant to their needs which they appear to enjoy exploring and interacting with?",
            (_) => """As well as being nourished, thermally comfortable, and rested, animals (like us) need to be physically and mentally occupied. """
        ),
        Question(QuestionIdentifier.Room.ordinal, Domain.InteractionsEnvironment, { 
            (a:Animal) => s"${a.name} has enough room to move around easily" },
            (a:Animal) => s"During times when ${a.name} is confined, do they have enough space to a move around in a way similar to their natural habitat? Depending on the species, this may include walking, running, climbing, flying",
            (_) => """From the animal’s perspective, anything smaller than a natural home range, is in principle, a compromise! But don’t give up, instead, you can apply the following rules of thumb: The longer an animal spends in an enclosure, the larger the enclosure  should be. The more choice (agency) the animal has to leave the enclosure, the more its management is aligned with their telos. Whenever confinement is imposed by the human carers or where the animal has no real choice, the general principle is that shorter periods of confinement are better."""
        ),
        Question(QuestionIdentifier.Exercise.ordinal, Domain.InteractionsEnvironment, { 
            (a:Animal) => s"${a.name} can choose to exercise and explore freely." },
            (a:Animal) => s"Does ${a.name} have the capacity to choose when and how to be active within their environment? Do they make use of available opportunities to explore and exercise?",
            (_) => """  Exploring occupies a animal’s time in a pleasurable and healthy way, and it has numerous benefits."""
        ),    
    ),
    Domain.InteractionsSocial -> Seq(
        Question(QuestionIdentifier.SameSpeciesInteraction.ordinal, Domain.InteractionsSocial, { 
            (a:Animal) => s"${a.name} can choose how they interact with other ${a.species.plural}." },
            (a:Animal) => s"Does ${a.name} have the capacity to choose if, when and how they interact with other ${a.species.plural}? Do they make use of available opportunities to spend time with other ${a.species.plural}?",
            (_) => """Retrieving data. Wait a few seconds and try to cut or copy again.By observing the choices the animal makes and their behaviour towards others, you can learn a lot about their subjective experience - and whether the interactions they engage in are giving rise to negative, neutral or positive experiences at least at the time of the assessment."""
        ),
        Question(QuestionIdentifier.OtherSpeciesInteraction.ordinal, Domain.InteractionsSocial, { 
            (a:Animal) => s"${a.name} has positive interactions with other ${a.species.plural}." },
            (a:Animal) => s"Are the interactions that ${a.name} has with other ${a.species.plural} generally positive? Are they able to establish social relationships and bonds similar to what would naturally occur with other ${a.species.plural}? ",
            (_) => """"Another reason to disagree with the statement is where the animal does not have the space, or the shape and design features of their housing do not allow them to avoid conflict with animals of other species who are present at the time of the assessment. """"
        ),
        Question(QuestionIdentifier.OtherSpeciesConflict.ordinal, Domain.InteractionsSocial, { 
            (a:Animal) => s"${a.name} can avoid conflicts with animals of other species." },
            (a:Animal) => s"Does ${a.name} have enough freedom of movement to be able to move away from or avoid situations of conflict with other species, such as potential predators? If ${a.name} is confined or tethered, or became cornered by another animal, this would not be possible.",
            (_) => """It is reasonable to say that, in most cases, from the animal’s perspective, maintaining a sense of safety during such interactions would be their very top priority."""
        ),    
    ),
    Domain.InteractionsHuman -> Seq(
        Question(QuestionIdentifier.CarerBond.ordinal, Domain.InteractionsHuman, { 
            (a:Animal) => s"${a.name} has healthy bonds with their carer(s)" },
            (a:Animal) => s"Does ${a.name} have a positive, trusting relationship with their carer(s) where they enjoy their company but are also able to enjoy spending time without them?",
            (_) => """Trust is associated with positive feelings of predictability and, it is built, maintained or eroded over time. Trust contributes to a sense of safety and security, and this is a positive subjective experience that is compatible with good welfare."""
        ),
        Question(QuestionIdentifier.FamiliarPeople.ordinal, Domain.InteractionsHuman, { 
            (a:Animal) => s"${a.name} interacts with familiar people confidently and safely" },
            (a:Animal) => s"Does ${a.name} appear calm, relaxed and confident when they are approached by or interact with familar people? Is their behaviour positive and consistent in this situation?",
            (_) => """During their lifetime, domestic animals often have to interact with many different humans – and every interaction will leave them feeling either negative, neutral or positive about the experience. It is important that animals generalize their trust, so they behave appropriately and safely with all humans, not just their main care providers."""
        ),    
        Question(QuestionIdentifier.CalmWithPeople.ordinal, Domain.InteractionsHuman, { 
            (a:Animal) => s"${a.name} is calm in the presence of most people" },
            (a:Animal) => s"Does ${a.name} appear calm when they are approached by or interact with unfamiliar people? Is their behaviour positive and consistent in this situation?",
            (_) => """We know that the absence of coercion and responding to subtle cues are the keys to an animal feeling in control, safe and confident, and these qualities are compatible with enhanced welfare,"""
        ),
    ),
)

lazy val questionMap = allQuestions.flatMap { case (s, qs) => qs.map(q => q.num -> q) }.toMap

val flattenedQs = for 
    (_, qs) <- allQuestions
    q <- qs
yield q

// The questions for a given domain
def domainQuestions(d:Domain) = flattenedQs.filter(_.domain == d)

enum Agreement:
    case StronglyDisagree
    case Disagree
    case Neutral
    case Agree
    case StronglyAgree

enum AnswerValue derives upickle.default.ReadWriter:
    case Numeric(value:Double)
    case Rated(value:Rating)

    def asDouble = this match {
        case Numeric(v) => v
        case Rated(r) => r.value
    }

    def categoryBase = Math.floor(asDouble / 20) * 20

    def categoryMidpoint = Math.floor(asDouble / 20) * 20 + 10


    def agreement:Agreement = 
        if asDouble <= 20 then Agreement.StronglyDisagree
        else if asDouble <= 40 then Agreement.Disagree
        else if asDouble <= 60 then Agreement.Neutral
        else if asDouble <= 80 then Agreement.Agree
        else Agreement.StronglyAgree

    def labelText = this match {
        case Numeric(v) => scoreText(v) //f" ${v * 100}%2.0f"
        case Rated(r) => scoreText(r)
    }

