module Tests.Helpers exposing (allPass, between1and, diceFuzzer, dieFuzzer, dieInitializers, passOrFail)

import Dice exposing (Dice)
import Die exposing (Die)
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


dieInitializers : List (Int -> Die)
dieInitializers =
    [ Die.d4, Die.d6, Die.d8, Die.d10 ]


dieFuzzer : Fuzzer Die
dieFuzzer =
    Fuzz.map2 (|>) Fuzz.int (Fuzz.oneOf (List.map Fuzz.constant dieInitializers))


diceFuzzer : Fuzzer Dice
diceFuzzer =
    Fuzz.map3
        Dice.init
        Fuzz.int
        (Fuzz.intRange 1 8)
        (Fuzz.oneOf (List.map Fuzz.constant dieInitializers))
