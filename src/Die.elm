module Die exposing (Die, Held, Rolled, cheat, face, generator, init, roll, size, sortValue, toString)

import Die.Size as Size exposing (Size(..))
import Random exposing (Generator, Seed)


type Die x
    = Held Size
    | Rolled Size Int


type Rolled
    = ReallyRolled Never


type Held
    = ReallyHeld Never


init : Size -> Die Held
init =
    Held


cheat : Size -> Int -> Die Rolled
cheat =
    Rolled


size : Die x -> Size
size die =
    case die of
        Held size_ ->
            size_

        Rolled size_ _ ->
            size_


generator : Die Held -> Generator (Die Rolled)
generator die =
    case die of
        Held size_ ->
            size_
                |> Size.toInt
                |> Random.int 1
                |> Random.map (die |> size |> Rolled)

        Rolled size_ value_ ->
            Random.constant (Rolled size_ value_)


roll : Seed -> Die Held -> Die Rolled
roll seed =
    generator
        >> Random.step
        >> (|>) seed
        >> Tuple.first


face : Die Rolled -> Int
face die =
    case die of
        Held _ ->
            0

        Rolled _ face_ ->
            face_


toString : Die x -> String
toString =
    size >> Size.toString


sortValue : Die x -> ( Int, Int )
sortValue die =
    case die of
        Held size_ ->
            ( 0, Size.toInt size_ |> negate )

        Rolled size_ value_ ->
            ( negate value_, Size.toInt size_ |> negate )
