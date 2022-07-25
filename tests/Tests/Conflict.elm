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
            , test "giving player does not retain dice if they cannot see" noSeeNoDiceOnGive
            ]
        , test "full playthrough on page 55" fullPlaythrough
        ]


fullPlaythrough : () -> Expectation
fullPlaythrough () =
    let
        you =
            Conflict.proponent

        me =
            Conflict.opponent
    in
    -- All told, you take up 6d6 plus 1d8, and I take up 8d6.
    Conflict.start
        -- You roll: 1 2 2 3 4 4 7.
        |> Result.andThen
            (Conflict.takeDice you
                ([ 1, 2, 2, 3, 4, 4 ]
                    |> List.foldl (Die.cheat D6 >> Dice.add) Dice.empty
                    |> Dice.add (Die.cheat D8 7)
                )
            )
        -- I roll: 1 1 1 3 4 5 6 6.
        |> Result.andThen
            (Conflict.takeDice me
                ([ 1, 1, 1, 3, 4, 5, 6, 6 ]
                    |> List.foldl (Die.cheat D6 >> Dice.add) Dice.empty
                )
            )
        -- You’re opening the conflict, so you start: “Hey, Zeke, you don’t just go shoot people,”
        -- you have your character say. “Let’s talk about this.” You Raise with a 4 and a 3, for 7.
        |> Result.andThen (Conflict.play you (Die.cheat D6 4))
        |> Result.andThen (Conflict.play you (Die.cheat D6 3))
        |> Result.andThen (Conflict.raise you)
        -- I put forward my own 4 and 3 to See. “Get out of my way, boy,” I have my character say.
        |> Result.andThen (Conflict.play me (Die.cheat D6 4))
        |> Result.andThen (Conflict.play me (Die.cheat D6 3))
        |> Result.andThen (Conflict.see me)
        -- “In fact, if you had any conscience of your own, you’d be with me.”
        -- That’s my Raise, so I put forward a 5 and a 6, for 11.
        |> Result.andThen (Conflict.play me (Die.cheat D6 5))
        |> Result.andThen (Conflict.play me (Die.cheat D6 6))
        |> Result.andThen (Conflict.raise me)
        -- You have my 11 to See, so you slide forward your 7 and your second 4.
        -- “Don’t try to tell me about my conscience,” you have your character say;
        -- that’s your See…
        |> Result.andThen (Conflict.play you (Die.cheat D8 7))
        |> Result.andThen (Conflict.play you (Die.cheat D6 4))
        |> Result.andThen (Conflict.see you)
        -- Here’s your Raise: “you go home and see to your son.”
        -- Raising with your best dice left: two 2s
        |> Result.andThen (Conflict.play you (Die.cheat D6 2))
        |> Result.andThen (Conflict.play you (Die.cheat D6 2))
        |> Result.andThen (Conflict.raise you)
        -- I see with my last 6, Reversing The Blow.
        -- “Ha! I remember how he used to look up to you!
        -- Maybe if you’d been in his life he wouldn’t have gone this way.”
        |> Result.andThen (Conflict.play me (Die.cheat D6 6))
        |> Result.andThen (Conflict.see me)
        -- Because I Reversed The Blow, I get to keep the 6 for my Raise. I add one of my 1s to it.
        |> Result.andThen (Conflict.play me (Die.cheat D6 1))
        |> Result.andThen (Conflict.raise me)
        -- “Forget this,” you say. “I punch you.”
        -- Let’s say that your character’s Body plus Will is 7d6.
        -- You roll: 1 3 4 5 5 5 6.
        |> Result.andThen
            (Conflict.takeDice you
                ([ 1, 3, 4, 5, 5, 5, 6 ]
                    |> List.foldl (Die.cheat D6 >> Dice.add) Dice.empty
                    -- Also, let’s say that your character has “Fist fighting 1d8” as a Trait,
                    -- so you roll that d8 as soon as you say “I punch you.”
                    -- You roll a 4 on the d8 and you still have that 1 left from before too.
                    |> Dice.add (Die.cheat D8 4)
                )
            )
        -- So you See my outstanding 7 with your 4 and your 3,
        |> Result.andThen (Conflict.play you (Die.cheat D6 4))
        |> Result.andThen (Conflict.play you (Die.cheat D6 3))
        |> Result.andThen (Conflict.see you)
        -- and put forward two of your 5s to Raise.
        |> Result.andThen (Conflict.play you (Die.cheat D6 5))
        |> Result.andThen (Conflict.play you (Die.cheat D6 5))
        |> Result.andThen (Conflict.raise you)
        -- Let’s say that my character’s Body plus Will is 6d6.
        -- I roll crap: 1 1 2 2 2 5. I have no immediately relevant Trait
        -- and my two leftover 1s aren’t much comfort.
        |> Result.andThen
            (Conflict.takeDice me
                ([ 1, 1, 2, 2, 2, 5 ]
                    |> List.foldl (Die.cheat D6 >> Dice.add) Dice.empty
                )
            )
        -- I have to See your 10. I See with my 5, two 2s and a 1.
        |> Result.andThen (Conflict.play me (Die.cheat D6 5))
        |> Result.andThen (Conflict.play me (Die.cheat D6 2))
        |> Result.andThen (Conflict.play me (Die.cheat D6 2))
        |> Result.andThen (Conflict.play me (Die.cheat D6 1))
        -- Because I’m Seeing with more than two dice, I’m Taking The Blow:
        -- “I’m surprised and you catch me right in the jaw,” I say.
        |> Result.andThen (Conflict.see me)
        -- I take four Fallout Dice, the number of dice I used to See,
        -- and since I took a punch they’re d6s. I set 4d6 aside for after the conflict.
        |> Result.andThen (Conflict.takeFallout me D6)
        -- Now all I have left to Raise with is a 2 and some 1s,
        -- and you have a 6, a 5, a 4 and some stuff. If I stay in the fight,
        -- you’ll beat the crap out of me. Instead I Give.
        |> Result.andThen (Conflict.give me)
        |> Expect.ok


noSeeNoDiceOnGive : () -> Expectation
noSeeNoDiceOnGive () =
    readiedConflictAfterRaise
        |> Result.andThen (Conflict.give Conflict.opponent)
        |> Result.map (Conflict.keptDie >> Expect.equal Nothing)
        |> Result.withDefault (Expect.fail "dice to keep when it should not be")


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
