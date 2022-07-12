module Dice exposing (Dice, combine, faces, init, roll, sizes, toList, toString)

import Die exposing (Die)
import Die.Size exposing (Size(..))
import Random


type Dice
    = Dice (List Die)


init : Int -> Int -> (Int -> Die) -> Dice
init seed count die =
    let
        dieListGen =
            Random.int Random.minInt Random.maxInt
                |> Random.map die
                |> Random.list count
    in
    Random.initialSeed seed
        |> Random.step dieListGen
        |> Tuple.first
        |> makeDice


toList : Dice -> List Die
toList (Dice list) =
    list


faces : Dice -> List Int
faces =
    toList
        >> List.map Die.face


combine : List Dice -> Dice
combine =
    List.concatMap toList >> makeDice


sizes : Dice -> List Size
sizes =
    toList >> List.map Die.size


roll : Dice -> Dice
roll =
    toList >> List.map Die.roll >> makeDice


makeDice : List Die -> Dice
makeDice =
    List.sortBy (Die.face >> negate) >> Dice


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
