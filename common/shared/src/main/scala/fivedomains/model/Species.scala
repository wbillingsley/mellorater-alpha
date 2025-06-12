package fivedomains.model

import upickle.default.{given, *}
import java.util.UUID

enum Species(val longText:String, val plural:String = "animals of their own species"):
    case Horse extends Species("Horse / Pony", "horses")
    case Dog extends Species("Dog", "dogs")
    case Cat extends Species("Cat", "cats")

    case Cattle extends Species("Cattle", "cattle")
    case Chicken extends Species("Chicken", "chickens")
    case Donkey extends Species("Donkey", "donkeys")
    case Goat extends Species("Goat", "goats")
    case Goose extends Species("Goose", "geese")
    case Duck extends Species("Duck", "ducks")
    case Mouse extends Species("Mouse", "mice")
    case Pig extends Species("Pig", "pigs")
    case RabbitDomestic extends Species("Rabbit", "rabbits")
    case Sheep extends Species("Sheep", "sheep")



    case Unknown extends Species(" ")


    // personal, professional, both
    // breeder, trainer/coach, competitor/exhibitor, vet, vet nurse, shelter worker, animal attendant, groomer, farmer/producer, inspector, academic/researcher, zookeeper, processor, other

    // rabbit, chicken, goose, duck, goat, cattle, sheep, fish, donkey, pig, mouse

    case AmphibianUnspecified extends Species("Amphibian (unspecified)")
    case Arthropod extends Species("Arthropod (unspecified)")
    case Budgerigar extends Species("Bird - Budgerigar")
    case Cockatiel extends Species("Bird - Cockatiel")
    case Cockatoo extends Species("Bird - Cockatoo")
    case BirdOther extends Species("Bird - other")
    case Parrot extends Species("Bird - Parrot (Psittacine)")
    case AfricanGrayParrot extends Species("Bird - Parrot (Psittacine) - African Grey Parrot")
    case AmazonParrot extends Species("Bird - Parrot (Psittacine) - Amazon Parrot")
    case Macaw extends Species("Bird - Parrot (Psittacine) - Macaw")
    case ParrotOther extends Species("Bird - Parrot (Psittacine) - other")
    case Parakeet extends Species("Bird - Parrot (Psittacine) - Parakeet")
    case Turkey extends Species("Bird - Turkey")
    case WaterfowlOther extends Species("Bird - Waterfowl - other")
    case Swan extends Species("Bird - Waterfowl - Swan")
    case Alpaca extends Species("Camelid - Alpaca")
    case Guanaco extends Species("Camelid - Guanaco")
    case Llama extends Species("Camelid - Llama")
    case CamelidOther extends Species("Camelid - other")
    case Marmoset extends Species("Common marmoset")
    case Deer extends Species("Deer (Cervidae)")
    case DuprasiGerbil extends Species("Duprasi (gerbil)")
    case Ferret extends Species("Ferret")
    case Goldfish extends Species("Fish - Coldwater - Carp, Goldfish")
    case CarpOther extends Species("Fish - Coldwater - Carp, other")
    case ColdwaterOther extends Species("Fish - Coldwater - other")
    case FishOther extends Species("Fish - other (unspecified)")
    case FishTropical extends Species("Fish - Tropical (unspecified)")
    case AfricanPygmyHedgehog extends Species("Hedgehog - African Pygmy (Atelerix albiventris) ")
    case EuropeanHedgehog extends Species("Hedgehog - European (Erinaceus europaeus) ")
    case HedgehogOther extends Species("Hedgehog - other")
    case Lion extends Species("Lion")
    case Mule extends Species("Mule (Equine)")
    case Panther extends Species("Panther")
    case RabbitUnspecified extends Species("Rabbit (unspecified)")
    case BeardedDragon extends Species("Reptile - Lizard - Bearded Dragon (Pogona vitticeps)")
    case Iguana extends Species("Reptile - Lizard - Iguana")
    case LeopardGecko extends Species("Reptile - Lizard - Leopard Gecko (Eublepharis macularius)")
    case LizardOther extends Species("Reptile - Lizard - other")
    case ReptileOther extends Species("Reptile - other")
    case Snake extends Species("Reptile - Snake")
    case Terrapin extends Species("Reptile - Terrapin")
    case Tortoise extends Species("Reptile - Tortoise")
    case HermannsTortoise extends Species("Reptile - Tortoise - Hermann's Tortoise (Testudo hermanni)")
    case MarginatedTortoise extends Species("Reptile - Tortoise - Marginated Tortoise (Testudo marginata)")
    case TortoiseOther extends Species("Reptile - Tortoise - other")
    case RussianTortoise extends Species("Reptile - Tortoise - Russian Tortoise (Testudo horsefeildii) ")
    case SpurThighedTortoise extends Species("Reptile - Tortoise - Spur Thighed Tortoise (Testudo graeca)")
    case Chinchilla extends Species("Rodent - Chinchilla")
    case Chipmunk extends Species("Rodent - Chipmunk")
    case Gerbil extends Species("Rodent - Gerbil")
    case GuineaPig extends Species("Rodent - Guinea Pig")
    case Hamster extends Species("Rodent - Hamster")
    case RodentOther extends Species("Rodent - other")
    case Rat extends Species("Rodent - Rat")
    case Wolf extends Species("Wolf")


object Species:
    /** As there's more than 32 species, we can't just derive a readwriter using the macros */
    given rw:ReadWriter[Species] = upickle.default.readwriter[String].bimap(
        s => s.longText,
        text => Species.values.find(_.longText == text).getOrElse(Species.Unknown)
    )

    def beta = Seq(Horse, Dog, Cat)
