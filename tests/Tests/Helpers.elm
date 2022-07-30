module Tests.Helpers exposing (allPass, between1and, passOrFail)

import Die.Size exposing (Size(..))
import Expect exposing (Expectation)


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
allPass expectations =
    expectations
        |> List.foldl passOrFail
            (expectations
                |> List.length
                |> Expect.atLeast 1
                |> Expect.onFail "there are no expectations"
            )
