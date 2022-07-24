module Tests.Conflict exposing (suite)

import Conflict exposing (Conflict, Error)
import Dice
import Die
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Pips
import Random
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Conflict"
        [ describe "taking dice"
            [ test "only rolled dice are accepted" onlyRolledDice
            , test "unrolled dice returns an error" unrolledError
            ]
        , describe "playing dice"
            [ test "dice from the correct side are accepted" playCorrectSide
            , test "dice from the wrong side are rejected" playWrongSide
            ]
        ]


playWrongSide : () -> Expectation
playWrongSide () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D8 7))
        |> Expect.err


playCorrectSide : () -> Expectation
playCorrectSide () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 5))
        |> Expect.ok


unrolledError : () -> Expectation
unrolledError () =
    let
        dice =
            Dice.init D6 Pips.four
    in
    Conflict.start
        |> Result.andThen (Conflict.takeDice Conflict.proponent dice)
        |> Expect.err


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



-- MOCKS


readiedConflict : Result Error Conflict
readiedConflict =
    let
        proponentDice =
            [ Die.cheat D6 5
            , Die.cheat D6 5
            , Die.cheat D6 2
            , Die.cheat D6 3
            ]
                |> List.foldl Dice.add Dice.empty

        opponentDice =
            [ Die.cheat D8 7
            , Die.cheat D8 3
            , Die.cheat D4 4
            ]
                |> List.foldl Dice.add Dice.empty
    in
    Conflict.start
        |> Result.andThen (Conflict.takeDice Conflict.proponent proponentDice)
        |> Result.andThen (Conflict.takeDice Conflict.opponent opponentDice)
