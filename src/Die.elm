module Die exposing (Die, d10, d4, d6, d8, lastRoll, roll, sides, toString)

import Random exposing (Seed)


type Die
    = Die Sides Seed Int


type Sides
    = D4
    | D6
    | D8
    | D10


init sides_ =
    Random.initialSeed >> Die sides_ >> (|>) 0


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


sides : Die -> Int
sides (Die d _ _) =
    sidesToInt d


sidesToInt : Sides -> Int
sidesToInt d =
    case d of
        D4 ->
            4

        D6 ->
            6

        D8 ->
            8

        D10 ->
            10


roll : Die -> Die
roll (Die sides_ seed_ _) =
    sidesToInt sides_
        |> Random.int 1
        |> Random.step
        |> (|>) seed_
        |> (\( newRoll, newSeed ) -> Die sides_ newSeed newRoll)


lastRoll : Die -> Int
lastRoll (Die _ _ roll_) =
    roll_


toString : Die -> String
toString =
    sides >> String.fromInt >> String.cons 'd'
