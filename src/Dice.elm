module Dice exposing (Dice, add, allRolled, combine, drop, empty, faces, generator, has, init, roll, sizes, toList, toString)

import Die exposing (Die)
import Die.Size exposing (Size(..))
import Pips exposing (Pips)
import Random exposing (Generator, Seed)


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


roll : Seed -> Dice -> Dice
roll seed =
    generator
        >> Random.step
        >> (|>) seed
        >> Tuple.first


makeDice : List Die -> Dice
makeDice =
    List.sortBy
        (\die ->
            ( Die.face die
                |> Maybe.withDefault 0
                |> negate
            , Die.size die
                |> Die.Size.toInt
                |> negate
            )
        )
        >> Dice


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


drop : Die -> Dice -> Dice
drop die =
    let
        drop_die list =
            case list of
                [] ->
                    []

                d :: rest ->
                    if d == die then
                        rest

                    else
                        d :: drop_die rest
    in
    toList >> drop_die >> makeDice


add : Die -> Dice -> Dice
add die =
    toList >> (::) die >> makeDice


has : Die -> Dice -> Bool
has die =
    toList >> List.member die


empty : Dice
empty =
    Dice []


allRolled : Dice -> Bool
allRolled =
    toList
        >> List.foldl
            (Die.face
                >> Maybe.map (always True)
                >> Maybe.withDefault False
                >> (&&)
            )
            True
