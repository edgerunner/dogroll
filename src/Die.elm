module Die exposing (Die, face, init, roll, size, toString)

import Die.Size as Size exposing (Size(..))
import Random exposing (Seed)


type Die
    = Die Size Seed


init : Size -> Int -> Die
init size_ =
    Random.initialSeed >> Die size_


size : Die -> Size
size (Die size_ _) =
    size_


seed : Die -> Seed
seed (Die _ seed_) =
    seed_


next : Die -> ( Int, Seed )
next die =
    size die
        |> Size.toInt
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
    size >> Size.toString
