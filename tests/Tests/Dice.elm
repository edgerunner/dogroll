module Tests.Dice exposing (..)

import Dice
import Die
import Expect exposing (Expectation)
import Test exposing (Test, describe, test, todo)


seed : Int
seed =
    42


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


suite : Test
suite =
    describe "Dice"
        [ test "initializing multiple dice"
            (\() ->
                Dice.init seed 4 Die.d6
                    |> Dice.faces
                    |> List.map (between1and 6)
                    |> List.foldl passOrFail Expect.pass
            )
        , todo "combining dice"
        , todo "rolling all dice at once"
        , todo "getting the best raise"
        ]


passOrFail : Expectation -> Expectation -> Expectation
passOrFail prev this =
    if this == Expect.pass && prev == Expect.pass then
        Expect.pass

    else if this == Expect.pass then
        prev

    else
        this
