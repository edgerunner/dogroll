module Die exposing (Die, Held, Rolled, cheat, face, generator, init, roll, size, sortValue, toString)

import Die.Size as Size exposing (Size(..))
import Random exposing (Generator, Seed)


type Die x
    = HeldDie Size
    | RolledDie Size Int


type Rolled
    = Rolled Rolled


type Held
    = Held Held


init : Size -> Die Held
init =
    HeldDie


cheat : Size -> Int -> Die Rolled
cheat =
    RolledDie


size : Die x -> Size
size die =
    case die of
        HeldDie size_ ->
            size_

        RolledDie size_ _ ->
            size_


generator : Die Held -> Generator (Die Rolled)
generator die =
    case die of
        HeldDie size_ ->
            size_
                |> Size.toInt
                |> Random.int 1
                |> Random.map (die |> size |> RolledDie)

        RolledDie size_ value_ ->
            Random.constant (RolledDie size_ value_)


roll : Seed -> Die Held -> Die Rolled
roll seed =
    generator
        >> Random.step
        >> (|>) seed
        >> Tuple.first


face : Die Rolled -> Int
face die =
    case die of
        HeldDie _ ->
            0

        RolledDie _ face_ ->
            face_


toString : Die x -> String
toString =
    size >> Size.toString


sortValue : Die x -> ( Int, Int )
sortValue die =
    case die of
        HeldDie size_ ->
            ( 0, Size.toInt size_ |> negate )

        RolledDie size_ value_ ->
            ( negate value_, Size.toInt size_ |> negate )
