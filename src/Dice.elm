module Dice exposing (Dice, combine, faces, init, sizes)

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


dieList : Dice -> List Die
dieList (Dice list) =
    list


faces : Dice -> List Int
faces =
    dieList
        >> List.map Die.face


combine : Dice -> Dice -> Dice
combine left right =
    [ left, right ]
        |> List.concatMap dieList
        |> Dice


sizes : Dice -> List Die.Size
sizes =
    dieList >> List.map Die.size
