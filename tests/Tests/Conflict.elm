module Tests.Conflict exposing (suite)

import Conflict
import Dice
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Pips
import Random
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Conflict"
        [ test "only rolled dice are accepted" onlyRolledDice
        ]


onlyRolledDice : () -> Expectation
onlyRolledDice () =
    let
        dice =
            Dice.init D6 Pips.four
                |> Dice.roll (Random.initialSeed 0)
    in
    Conflict.start
        |> Result.andThen (Conflict.takeDice Conflict.proponent dice)
        |> Expect.ok
