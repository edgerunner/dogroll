module Die exposing (Die, face, generator, init, size, toString)

import Die.Size as Size exposing (Size(..))
import Random exposing (Generator)


type Die
    = Held Size
    | Rolled Size Int


init : Size -> Die
init =
    Held


size : Die -> Size
size die =
    case die of
        Held size_ ->
            size_

        Rolled size_ _ ->
            size_


generator : Die -> Generator Die
generator die =
    die
        |> size
        |> Size.toInt
        |> Random.int 1
        |> Random.map (die |> size |> Rolled)


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
