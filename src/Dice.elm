module Dice exposing (Dice, faces, init)

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
