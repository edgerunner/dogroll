module Die exposing (Die, cheat, face, generator, init, roll, size, toString)

import Die.Size as Size exposing (Size(..))
import Random exposing (Generator, Seed)


type Die
    = Held Size
    | Rolled Size Int


init : Size -> Die
init =
    Held


cheat : Size -> Int -> Die
cheat =
    Rolled


size : Die -> Size
size die =
    case die of
        Held size_ ->
            size_

        Rolled size_ _ ->
            size_


generator : Die -> Generator Die
generator die =
    case die of
        Held size_ ->
            size_
                |> Size.toInt
                |> Random.int 1
                |> Random.map (die |> size |> Rolled)

        Rolled _ _ ->
            Random.constant die


roll : Seed -> Die -> Die
roll seed =
    generator
        >> Random.step
        >> (|>) seed
        >> Tuple.first


face : Die -> Maybe Int
face die =
    case die of
        Held _ ->
            Nothing

        Rolled _ face_ ->
            Just face_


toString : Die -> String
toString =
    size >> Size.toString
