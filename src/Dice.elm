module Dice exposing (Dice, combine, faces, init, roll, sizes, toList, toString)

import Die exposing (Die)
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


sizes : Dice -> List Die.Size
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
            toList
                >> List.foldl
                    (\die rec ->
                        case Die.size die of
                            Die.D4 ->
                                { rec | d4 = rec.d4 + 1 }

                            Die.D6 ->
                                { rec | d6 = rec.d6 + 1 }

                            Die.D8 ->
                                { rec | d8 = rec.d8 + 1 }

                            Die.D10 ->
                                { rec | d10 = rec.d10 + 1 }
                    )
                    { d4 = 0, d6 = 0, d8 = 0, d10 = 0 }

        sizesToString { d4, d6, d8, d10 } =
            [ sizeToString "d10" d10
            , sizeToString "d8" d8
            , sizeToString "d6" d6
            , sizeToString "d4" d4
            ]
                |> List.filter ((/=) "")

        sizeToString name count =
            if count < 1 then
                ""

            else
                String.fromInt count ++ name
    in
    countSizes >> sizesToString >> String.join "+"
