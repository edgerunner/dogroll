module Dice.Type exposing (..)

import Dice exposing (Dice)
import Die.Size exposing (Size(..))


type Type
    = Stat Stat (List ())
    | Trait String Size (List ())
    | Relationship String Size (List ())
    | Belonging String Quality Gun


type Stat
    = Acuity
    | Heart
    | Body
    | Will


type Gun
    = Gun
    | NotGun


type Quality
    = Normal
    | Excellent
    | Big
    | ExcellentPlusBig
    | Crap


toDice : Int -> Type -> Dice
toDice seed type_ =
    let
        addGunD4 base =
            case type_ of
                Belonging _ _ Gun ->
                    Dice.combine [ base, Dice.init (seed - 1) 1 D4 ]

                _ ->
                    base
    in
    Dice.init
        seed
        (count type_)
        (size type_)
        |> addGunD4


name : Type -> String
name type_ =
    case type_ of
        Stat stat _ ->
            case stat of
                Acuity ->
                    "Acuity"

                Heart ->
                    "Heart"

                Body ->
                    "Body"

                Will ->
                    "Will"

        Trait name_ _ _ ->
            name_

        Relationship name_ _ _ ->
            name_

        Belonging name_ _ _ ->
            name_


size : Type -> Size
size type_ =
    case type_ of
        Stat _ _ ->
            D6

        Trait _ size_ _ ->
            size_

        Relationship _ size_ _ ->
            size_

        Belonging _ quality _ ->
            case quality of
                Normal ->
                    D6

                Excellent ->
                    D6

                Big ->
                    D8

                ExcellentPlusBig ->
                    D8

                Crap ->
                    D4


count : Type -> Int
count type_ =
    case type_ of
        Stat _ pips ->
            List.length pips + 2

        Trait _ _ pips ->
            List.length pips + 1

        Relationship _ _ pips ->
            List.length pips + 1

        Belonging _ quality _ ->
            case quality of
                Normal ->
                    1

                Excellent ->
                    2

                Big ->
                    1

                ExcellentPlusBig ->
                    2

                Crap ->
                    1
