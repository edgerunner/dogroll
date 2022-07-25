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
            [ test "raising player can raise after playing two dice" raiseAfterTwoDice
            , test "raising player can't raise after playing one die" raiseAfterOneDie
            , test "raising player can't raise without playing dice" raiseAfterNoDice
            , test "other player can't raise at all" otherPlayerCantRaise
            , test "other player can't even play dice" otherPlayerCantPlayDice
            , test "player can't raise with more than two dice" raiseWithMoreThanTwoDice
            , test "player can't raise twice" raiseTwice
            ]
        , describe "seeing"
            [ test "seeing player should be able to play dice after the raise" seeingPlayerCanPlayDiceToSee
            , test "seeing player can play any number of dice" seeingPlayerCanPlayAnyNumberOfDice
            , test "see total needs to be at least the raise total" cantSeeWithLowTotal
            , test "seeing player can see if the total meets the raise total" seeingPlayerCanSeeIfTotalMeetsRaiseTotal
            , test "raising player can't see their own raise" cantSeeOwnRaise
            , describe "seeing outcomes"
                [ test "reversing the blow: single die is kept for next raise" seeWithSingleDieIsReversingBlow
                , test "block or dodge: seeing player proceeds to raise normally" seeWithBlockOrDodge
                , describe "taking the blow"
                    [ test "seeing player must choose fallout die size" takeBlowAndFalloutDice
                    , test "seeing player can't continue to raise before choosing fallout die size" cantTakeBlowWithoutFalloutDice
                    ]
                ]
            ]
        , describe "giving"
            [ test "giving player can give on their own turn" giveOnOwnTurn
            , test "giving player can't give on other player's turn" cantGiveOnOtherPlayerTurn
            , test "giving player retains their best dice from the conflict if gives in their raise" giveRetainsBestDice
            ]
        ]


giveRetainsBestDice : () -> Expectation
giveRetainsBestDice () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 3))
        |> Result.andThen (Conflict.see Conflict.opponent)
        |> Result.andThen (Conflict.give Conflict.opponent)
        |> Result.map (Conflict.keptDie >> Expect.equal (Die.cheat D8 7 |> Just))
        |> Result.withDefault (Expect.fail "no dice to keep")


cantGiveOnOtherPlayerTurn : () -> Expectation
cantGiveOnOtherPlayerTurn () =
    readiedConflict
        |> Result.andThen (Conflict.give Conflict.opponent)
        |> Expect.err


giveOnOwnTurn : () -> Expectation
giveOnOwnTurn () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.give Conflict.opponent)
        |> Expect.ok


cantTakeBlowWithoutFalloutDice : () -> Expectation
cantTakeBlowWithoutFalloutDice () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 3))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.see Conflict.opponent)
        |> Result.andThen (Conflict.takeDice Conflict.opponent (Dice.empty |> Dice.add (Die.cheat D10 7)))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D10 7))
        |> Expect.err


takeBlowAndFalloutDice : () -> Expectation
takeBlowAndFalloutDice () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 3))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.see Conflict.opponent)
        |> Result.andThen (Conflict.takeFallout Conflict.opponent D6)
        |> Result.map (Conflict.state >> .opponent >> .fallout)
        |> Result.map (Expect.equal (Dice.init D6 Pips.three))
        |> Result.withDefault (Expect.fail "didn't take blow")


seeWithBlockOrDodge : () -> Expectation
seeWithBlockOrDodge () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 3))
        |> Result.andThen (Conflict.see Conflict.opponent)
        |> Result.andThen (Conflict.takeDice Conflict.opponent (Dice.empty |> Dice.add (Die.cheat D10 9)))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D10 9))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.raise Conflict.opponent)
        |> Expect.ok


seeWithSingleDieIsReversingBlow : () -> Expectation
seeWithSingleDieIsReversingBlow () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.takeDice Conflict.opponent (Dice.empty |> Dice.add (Die.cheat D10 9)))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D10 9))
        |> Result.andThen (Conflict.see Conflict.opponent)
        -- can see with just one more die, the single see die is transferred
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.raise Conflict.opponent)
        |> Expect.ok


cantSeeOwnRaise : () -> Expectation
cantSeeOwnRaise () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.see Conflict.proponent)
        |> Expect.err


seeingPlayerCanSeeIfTotalMeetsRaiseTotal : () -> Expectation
seeingPlayerCanSeeIfTotalMeetsRaiseTotal () =
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


seeingPlayerCanPlayAnyNumberOfDice : () -> Expectation
seeingPlayerCanPlayAnyNumberOfDice () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 7))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D8 3))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D4 4))
        |> Result.andThen (Conflict.takeDice Conflict.opponent (Dice.add (Die.cheat D6 1) Dice.empty))
        |> Result.andThen (Conflict.play Conflict.opponent (Die.cheat D6 1))
        |> Expect.ok


seeingPlayerCanPlayDiceToSee : () -> Expectation
seeingPlayerCanPlayDiceToSee () =
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
