module Dice exposing (Dice, add, best, combine, count, drop, empty, faces, generator, has, init, match, roll, sizes, toList, toString, total)

import Die exposing (Die, Held, Rolled)
import Die.Size exposing (Size(..))
import Pips exposing (Pips)
import Random exposing (Generator, Seed)


type Dice x
    = Dice (List (Die x))


init : Size -> Pips -> Dice Held
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


count : Dice x -> Int
count =
    toList >> List.length


combine : List (Dice x) -> Dice x
combine =
    List.concatMap toList >> makeDice


sizes : Dice x -> List Size
sizes =
    toList >> List.map Die.size


generator : Dice Held -> Generator (Dice Rolled)
generator =
    toList
        >> List.map Die.generator
        >> List.foldl (Random.map2 (::)) (Random.constant [])
        >> Random.map makeDice


roll : Seed -> Dice Held -> Dice Rolled
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

        sizeToString size count_ =
            if count_ < 1 then
                ""

            else
                String.fromInt count_
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


best : Int -> Dice x -> Dice x
best count_ =
    toList >> List.take count_ >> makeDice


match : Int -> Dice Rolled -> Dice Rolled
match threshold =
    toList
        >> subsequences
        >> List.map (List.foldl add empty)
        >> List.sortBy (\dice -> ( total dice, count dice ))
        >> List.foldr
            (\subset selected ->
                if total subset >= threshold then
                    subset

                else
                    selected
            )
            empty



-- Taken from https://github.com/elm-community/list-extra/blob/8.6.0/src/List/Extra.elm#L1039


subsequences : List a -> List (List a)
subsequences list =
    case list of
        [] ->
            []

        first :: rest ->
            let
                f ys r =
                    ys :: (first :: ys) :: r
            in
            [ first ] :: List.foldr f [] (subsequences rest)
