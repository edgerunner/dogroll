module Tests.Helpers exposing (allPass, between1and, combinedDiceFuzzer, diceFuzzer, dieFuzzer, passOrFail)

import Dice exposing (Dice)
import Dice.Pips exposing (Pips)
import Die exposing (Die)
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)


passOrFail : Expectation -> Expectation -> Expectation
passOrFail prev this =
    if this == Expect.pass && prev == Expect.pass then
        Expect.pass

    else if this == Expect.pass then
        prev

    else
        this


between1and : Int -> Int -> Expectation
between1and n value =
    if value >= 1 && value <= n then
        Expect.pass

    else
        [ String.fromInt value
        , " is not between 1 and "
        , String.fromInt n
        ]
            |> String.concat
            |> Expect.fail


allPass : List Expectation -> Expectation
allPass =
    List.foldl passOrFail Expect.pass


dieFuzzer : Fuzzer Die
dieFuzzer =
    Fuzz.map
        Die.init
        (Fuzz.oneOf (List.map Fuzz.constant Die.Size.all))


pipsFuzzer : Int -> Fuzzer Pips
pipsFuzzer =
    Fuzz.intRange 1 >> Fuzz.map Dice.Pips.fromInt


diceFuzzer : Fuzzer Dice
diceFuzzer =
    Fuzz.map2
        Dice.init
        sizeFuzzer
        (pipsFuzzer 8)


sizeFuzzer : Fuzzer Size
sizeFuzzer =
    Fuzz.oneOf (List.map Fuzz.constant Die.Size.all)


combinedDiceFuzzer : Fuzzer Dice
combinedDiceFuzzer =
    Fuzz.list diceFuzzer
        |> Fuzz.map (List.take 4 >> Dice.combine)
