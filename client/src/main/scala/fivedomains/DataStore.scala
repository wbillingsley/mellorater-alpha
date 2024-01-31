package fivedomains

import scala.collection.mutable
import com.wbillingsley.veautiful.PushVariable

import org.scalajs.dom.window.localStorage 
import model.{given, *}

// Pickling library, for stringifying data to store in localStorage
import upickle.default.*
// Automatic writers for case classes
import upickle.default.{ReadWriter => RW, macroRW}
import typings.std.stdStrings.a

import fivedomains.model.Confidence
import java.util.UUID



given RW[Animal] = macroRW
given RW[Confidence] = macroRW
given RW[Answer] = macroRW
//given RW[Situation] = RW.merge(macroRW[Situation.Competition], macroRW[Situation.DayToDay], macroRW[Situation.Exercising], macroRW[Situation.Showing], macroRW[Situation.Training], macroRW[Situation.Transport], macroRW[Situation.Veterinary], macroRW[Situation.Working])
given RW[Assessment] = macroRW

case class DataBlob(
    acceptedSensitiveTopics:Boolean,
    animalMap:mutable.Map[AnimalId, Animal] = mutable.Map.empty[AnimalId, Animal],
    assessments:mutable.Buffer[Assessment] = mutable.Buffer.empty[Assessment]
)  
 
/** A data store for the five domains app */
object DataStore {

    val acceptedSensitiveTopics = PushVariable(
        Option(localStorage.getItem("acceptedSensitiveTopics")).map(read[Boolean](_)).getOrElse(false)
    ) { value => 
        localStorage.setItem("acceptedSensitiveTopics", write(value))
        if animals.isEmpty then 
            Router.routeTo(AppRoute.AddAnimal) 
        else
            Router.routeTo(AppRoute.Front)
    }



    val animalMap:mutable.Map[AnimalId, Animal] =
        val stored = Option(localStorage.getItem("animalMap"))
        stored match {
            case Some(json) =>
                val parsed = read[Map[AnimalId, Animal]](json) 
                parsed.to(mutable.Map)
                
            case None => mutable.Map.empty[AnimalId, Animal] 
        }
    

    def animal(a:AnimalId) = animalMap(a)

    def addAnimal(a:Animal):Animal =
        animalMap(a.id) = a
        localStorage.setItem("animalMap", write(animalMap))
        a

    def nextAnimalId = UUID.randomUUID()

    private val _assessments:mutable.Buffer[Assessment] = 
        val stored = Option(localStorage.getItem("assessments"))
        stored match {
            case Some(json) =>
                val parsed = read[Seq[Assessment]](json) 
                parsed.to(mutable.Buffer)
                //mutable.Buffer.empty[Assessment]
            case None => 
                mutable.Buffer.empty[Assessment]
        }
        

    /** Saves a new assessment to memory and JSON */
    def addAssessment(a:Assessment):Unit = 
        _assessments.append(a)
        localStorage.setItem("assessments", write(assessments))

    def addAssessment(animal:AnimalId, situation:Situation, time:Double, answers:Seq[(AnswerValue, Confidence, Option[String])]):Unit = 
        _assessments.append(Assessment(animal=animal, situation=situation, time=time, 
          (for ((ans, conf, note), i) <- answers.zipWithIndex yield i -> Answer(i, ans, conf, note)).toMap
        ))
        localStorage.setItem("assessments", write(assessments))

    def assessments = _assessments.toSeq

    def surveysFor(a:Animal) = assessments.toSeq.filter(_.animal == a.id)

    def animals = animalMap.values.toSeq.sortBy(_.id)

    def clearAll() = 
        _assessments.clear()
        animalMap.clear()
        localStorage.setItem("assessments", write(assessments))
        localStorage.setItem("animalMap", write(animalMap))

}