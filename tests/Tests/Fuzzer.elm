module Tests.Fuzzer exposing (..)

import Dice exposing (Dice)
import Die exposing (Die, Held, Rolled)
import Die.Size exposing (Size)
import Fuzz exposing (Fuzzer)
import Pips exposing (Pips)
import Random exposing (Seed)
import Shrink


rolledDice : Fuzzer (Dice Held) -> Fuzzer (Dice Rolled)
rolledDice =
    Fuzz.map2 Dice.roll seed


seed : Fuzzer Seed
seed =
    Fuzz.custom Random.independentSeed Shrink.noShrink


combinedDice : Fuzzer (Dice Held)
combinedDice =
    Fuzz.constant []
        |> Fuzz.map2 (::) dice
        |> Fuzz.map2 (::) dice
        |> Fuzz.map2 (::) dice
        |> Fuzz.map2 (::) dice
        |> Fuzz.map Dice.combine


size : Fuzzer Size
size =
    Fuzz.oneOf (List.map Fuzz.constant Die.Size.all)


dice : Fuzzer (Dice Held)
dice =
    Fuzz.map2
        Dice.init
        size
        (pips 8)


pips : Int -> Fuzzer Pips
pips =
    Fuzz.intRange 1 >> Fuzz.map Pips.fromInt


die : Fuzzer (Die Held)
die =
    Fuzz.map
        Die.init
        (Fuzz.oneOf (List.map Fuzz.constant Die.Size.all))
