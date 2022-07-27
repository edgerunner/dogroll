module Dice exposing (Dice, add, combine, drop, empty, faces, generator, has, init, roll, sizes, toList, toString, total)

import Die exposing (Die, Rolled)
import Die.Size exposing (Size(..))
import Pips exposing (Pips)
import Random exposing (Generator, Seed)


type Dice x
    = Dice (List (Die x))


init : Size -> Pips -> Dice x
init size =
    Pips.repeat (Die.init size) >> Dice


toList : Dice x -> List (Die x)
toList (Dice list) =
    list


faces : Dice Rolled -> List Int
faces =
    toList
        >> List.map Die.face


total : Dice Rolled -> Int
total =
    faces >> List.sum


combine : List (Dice x) -> Dice x
combine =
    List.concatMap toList >> makeDice


sizes : Dice x -> List Size
sizes =
    toList >> List.map Die.size


generator : Dice x -> Generator (Dice Rolled)
generator =
    toList
        >> List.map Die.generator
        >> List.foldl (Random.map2 (::)) (Random.constant [])
        >> Random.map makeDice


roll : Seed -> Dice x -> Dice Rolled
roll seed =
    generator
        >> Random.step
        >> (|>) seed
        >> Tuple.first


makeDice : List (Die x) -> Dice x
makeDice =
    List.sortBy
        Die.sortValue
        >> Dice


toString : Dice x -> String
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


drop : Die x -> Dice x -> Dice x
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


add : Die x -> Dice x -> Dice x
add die =
    toList >> (::) die >> makeDice


has : Die x -> Dice x -> Bool
has die =
    toList >> List.member die


empty : Dice x
empty =
    Dice []
