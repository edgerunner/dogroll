module Die exposing (Die, d10, d4, d6, d8, lastRoll, roll, sides, toString)

import Random exposing (Seed)


type Die
    = Die Sides Seed Roll


type alias Sides =
    Int


type alias Roll =
    Int


init : Sides -> Int -> Die
init sides_ =
    Random.initialSeed >> Die sides_ >> (|>) 0 >> roll


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
sides (Die sides_ _ _) =
    sides_


roll : Die -> Die
roll (Die sides_ seed_ _) =
    Random.int 1 sides_
        |> Random.step
        |> (|>) seed_
        |> (\( newRoll, newSeed ) -> Die sides_ newSeed newRoll)


lastRoll : Die -> Int
lastRoll (Die _ _ roll_) =
    roll_


toString : Die -> String
toString =
    sides >> String.fromInt >> String.cons 'd'
