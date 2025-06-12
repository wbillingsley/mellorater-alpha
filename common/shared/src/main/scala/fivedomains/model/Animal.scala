package fivedomains.model

import upickle.default.ReadWriter
import java.util.UUID

/** The display style is purely decorative, as placeholder because at the moment we don't have iconography to make animals disinct */
enum DisplayStyle derives ReadWriter:
    case Curls
    case Herringbone
    case Flowers
    case Circles
    case Plaid

object DisplayStyle:
    def random = DisplayStyle.fromOrdinal(scala.util.Random.nextInt(DisplayStyle.values.length))

/** Used internally to track animals etc */
type AnimalId = UUID

object AnimalId {
    def unapply(s:String):Option[AnimalId] = try {
        Option(UUID.fromString(s))
    } catch {
        case _ => None
    }
}


enum Sex derives ReadWriter:
    case Male
    case Female
    case MaleNeutered
    case FemaleNeutered
    case Unspecified


extension (s:Sex) {
    def prettyString = s match
        case Sex.MaleNeutered => "Male neutered"
        case Sex.FemaleNeutered => "Female neutered"
        case _ => s.toString
    
}

case class Animal(
    id:AnimalId, 
    name:String, 
    species:Species,
    breed:String = "",
    sex:Sex = Sex.Unspecified, 
    desexed:Option[Long] = None,
    display:DisplayStyle = DisplayStyle.random,
    testData:Boolean = false
) {

    def displayName = if name.nonEmpty then name else "Unnamed animal"
}


