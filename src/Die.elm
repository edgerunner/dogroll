module Die exposing (Die, d10, d4, d6, d8, face, roll, sides, toString)

import Random exposing (Seed)


type Die
    = Die Sides Seed


type alias Sides =
    Int


init : Sides -> Int -> Die
init sides_ =
    Random.initialSeed >> Die sides_


d4 : Int -> Die
d4 =
    init 4


d6 : Int -> Die
d6 =
    init 6


d8 : Int -> Die
d8 =
    init 8


d10 : Int -> Die
d10 =
    init 10


sides : Die -> Int
sides (Die sides_ _) =
    sides_


next : Die -> ( Int, Seed )
next (Die sides_ seed_) =
    Random.int 1 sides_
        |> Random.step
        |> (|>) seed_


roll : Die -> Die
roll die =
    next die
        |> Tuple.second
        |> Die (sides die)


face : Die -> Int
face =
    next >> Tuple.first


toString : Die -> String
toString =
    sides >> String.fromInt >> String.cons 'd'
