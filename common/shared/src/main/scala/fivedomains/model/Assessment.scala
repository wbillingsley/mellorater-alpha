package fivedomains.model

case class Assessment(animal:AnimalId, situation:Situation, time:Double, answers:Map[Int, Answer]) {

    // /** Average score in a given domain */
    // def average(domain:Domain):Option[Double = domain match {
    //     case Domain.Mental => 
    //         answers.values.map(_.value.asDouble).sum / answers.size
    //     case d => 
    //         val filtered = answers.values.filter { (a) => flattenedQs.find(_.num == a.q).map(_.domain).contains(d) }
    //         filtered.map(_.value.asDouble).sum / filtered.size
    // }

    def answersBelow(level:Double) = answers.values.filter(_.value.asDouble <= level)

    // Answers rated poor or worse
    lazy val lowAnswers = answersBelow(Rating.Poor.value)

    lazy val domainsContainingConcern = Domain.values.filter(d => lowAnswers.exists(a => a.question.domain == d))

    def overallConfidence = answers.values.map(_.confidence.value).sum / answers.size

    def answer(q:Question) = answers(q.num)

    def answerOpt(q:Question) = answers.get(q.num)

    def heuristic(numbers:Iterable[Double]):Double = 
        if numbers.isEmpty then 0 else 
            val minScore = numbers.min
            val avgScore = numbers.sum / numbers.size
            Math.min(avgScore, minScore + 20)

    def answersInDomain(d:Domain) = 
        for 
            q <- domainQuestions(d)
            a <- answers.get(q.num)
        yield a

    /** Whether all the questions in a domain have been completed */
    def completed(d:Domain):Boolean = 
        domainQuestions(d).forall((q) => answers.get(q.num).nonEmpty)

    def categoryScore(d:Domain):Option[Double] = 
        if completed(d) then 
            val filtered = answers.values.filter { (a) => flattenedQs.find(_.num == a.q).map(_.domain).contains(d) }
            val numeric = filtered.map(_.value.categoryMidpoint)
            Some(heuristic(numeric))
        else None

    def overallScore:Option[Double] = 
        val catScores = for d <- Domain.scoredDomains yield categoryScore(d)
        if catScores.exists(_.isEmpty) then None else Some(heuristic(catScores.flatten))

}

extension (s:Iterable[Assessment]) {
    def overallConfidence = s.map(_.overallConfidence).sum / s.size
}