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
            , test "totally unrelated dice are rejected" playUnrelatedDice
            ]
        , describe "raising"
            [ test "player can raise after playing two dice" raiseAfterTwoDice
            , test "player can't raise after playing one die" raiseAfterOneDie
            , test "player can't raise without playing dice" raiseAfterNoDice
            , test "other player can't raise at all" otherPlayerCantRaise
            , test "other player can't even play dice" otherPlayerCantPlayDice
            , test "player can't raise with more than two dice" raiseWithMoreThanTwoDice
            , test "player can't raise twice" raiseTwice
            ]
        , describe "seeing"
            [ test "other player should be able to play dice after the raise" otherPlayerCanPlayDiceToSee
            , test "other player can play any number of dice" otherPlayerCanPlayAnyNumberOfDice
            , test "see total needs to be at least the raise total" cantSeeWithLowTotal
            , test "other player can see if the total meets the raise total" otherPlayerCanSeeIfTotalMeetsRaiseTotal
            , test "raising player can't see their own raise" cantSeeOwnRaise
            ]
        ]


cantSeeOwnRaise : () -> Expectation
cantSeeOwnRaise () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.see Conflict.proponent)
        |> Expect.err


otherPlayerCanSeeIfTotalMeetsRaiseTotal : () -> Expectation
otherPlayerCanSeeIfTotalMeetsRaiseTotal () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.see Conflict.opponent)
        |> Expect.ok


cantSeeWithLowTotal : () -> Expectation
cantSeeWithLowTotal () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 3))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.see Conflict.opponent)
        |> Expect.err


otherPlayerCanPlayAnyNumberOfDice : () -> Expectation
otherPlayerCanPlayAnyNumberOfDice () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 3))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.takeDice Conflict.opponent (Dice.add (Die.cheat D6 1) Dice.empty))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D6 1))
        |> Expect.ok


otherPlayerCanPlayDiceToSee : () -> Expectation
otherPlayerCanPlayDiceToSee () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Expect.ok


raiseTwice : () -> Expectation
raiseTwice () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 5))
        |> Result.andThen (Conflict.raise Conflict.proponent)
        |> Result.andThen (Conflict.raise Conflict.proponent)
        |> Expect.err


raiseWithMoreThanTwoDice : () -> Expectation
raiseWithMoreThanTwoDice () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 5))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 2))
        |> Expect.err


otherPlayerCantPlayDice : () -> Expectation
otherPlayerCantPlayDice () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Expect.err


otherPlayerCantRaise : () -> Expectation
otherPlayerCantRaise () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 5))
        |> Result.andThen (Conflict.raise Conflict.opponent)
        |> Expect.err


raiseAfterNoDice : () -> Expectation
raiseAfterNoDice () =
    readiedConflict
        |> Result.andThen (Conflict.raise Conflict.proponent)
        |> Expect.err


raiseAfterOneDie : () -> Expectation
raiseAfterOneDie () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.raise Conflict.proponent)
        |> Expect.err


raiseAfterTwoDice : () -> Expectation
raiseAfterTwoDice () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 5))
        |> Result.andThen (Conflict.raise Conflict.proponent)
        |> Expect.ok


playUnrelatedDice : () -> Expectation
playUnrelatedDice () =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D10 2))
        |> Expect.err


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


readiedConflictAfterRaise : Result Error Conflict
readiedConflictAfterRaise =
    readiedConflict
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 3))
        |> Result.andThen (Conflict.play Conflict.proponent (Die.cheat D6 5))
        |> Result.andThen (Conflict.raise Conflict.proponent)
