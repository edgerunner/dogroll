module Dice exposing (Dice, combine, faces, generator, init, sizes, toList, toString)

import Dice.Pips as Pips exposing (Pips)
import Die exposing (Die)
import Die.Size exposing (Size(..))
import Random exposing (Generator)


type Dice
    = Dice (List Die)


init : Size -> Pips -> Dice
init size =
    Pips.repeat (Die.init size) >> Dice


toList : Dice -> List Die
toList (Dice list) =
    list


faces : Dice -> List Int
faces =
    toList
        >> List.filterMap Die.face


combine : List Dice -> Dice
combine =
    List.concatMap toList >> makeDice


sizes : Dice -> List Size
sizes =
    toList >> List.map Die.size


generator : Dice -> Generator Dice
generator =
    toList
        >> List.map Die.generator
        >> List.foldl (Random.map2 (::)) (Random.constant [])
        >> Random.map makeDice


makeDice : List Die -> Dice
makeDice =
    List.sortBy (Die.face >> Maybe.withDefault 0 >> negate) >> Dice


toString : Dice -> String
toString =
    let
        countSizes =
            sizes >> Die.Size.count

        sizesToString { d4, d6, d8, d10 } =
            [ sizeToString D10 d10
            , sizeToString D8 d8
            , sizeToString D6 d6
            , sizeToString D4 d4
            ]
                |> List.filter ((/=) "")

        sizeToString size count =
            if count < 1 then
                ""

            else
                String.fromInt count
                    ++ Die.Size.toString size
    in
    countSizes >> sizesToString >> String.join "+"
