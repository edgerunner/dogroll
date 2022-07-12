module Dice exposing (Dice, combine, faces, init, roll, sizes, toList)

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
        |> Dice


toList : Dice -> List Die
toList (Dice list) =
    list


faces : Dice -> List Int
faces =
    toList
        >> List.map Die.face


combine : List Dice -> Dice
combine =
    List.concatMap toList >> Dice


sizes : Dice -> List Die.Size
sizes =
    toList >> List.map Die.size


roll : Dice -> Dice
roll =
    toList >> List.map Die.roll >> Dice
