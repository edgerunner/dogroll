module Die exposing (Die, Size(..), d10, d4, d6, d8, face, faces, roll, size, toString)

import Random exposing (Seed)


type Die
    = Die Size Seed


type Size
    = D4
    | D6
    | D8
    | D10


init : Size -> Int -> Die
init sides_ =
    Random.initialSeed >> Die sides_


d4 : Int -> Die
d4 =
    init D4


d6 : Int -> Die
d6 =
    init D6


d8 : Int -> Die
d8 =
    init D8


d10 : Int -> Die
d10 =
    init D10


size : Die -> Size
size (Die sides_ _) =
    sides_


faces : Die -> Int
faces die =
    case size die of
        D4 ->
            4

        D6 ->
            6

        D8 ->
            8

        D10 ->
            10


seed : Die -> Seed
seed (Die _ seed_) =
    seed_


next : Die -> ( Int, Seed )
next die =
    faces die
        |> Random.int 1
        |> Random.step
        |> (|>) (seed die)


roll : Die -> Die
roll die =
    next die
        |> Tuple.second
        |> Die (size die)


face : Die -> Int
face =
    next >> Tuple.first


toString : Die -> String
toString =
    faces >> String.fromInt >> String.cons 'd'
