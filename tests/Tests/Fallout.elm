module Tests.Fallout exposing (suite)

import Dice
import Die
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Fallout exposing (Outcome(..), State(..))
import Pips
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Fallout"
        [ describe "taking dice"
            [ test "dice should be piled together" diceShouldBePiledTogether
            , test "dice can't be taken after a roll" diceCantBeTakenAfterRoll
            ]
        , describe "rolling dice"
            [ test "pending dice can be rolled" pendingDiceCanBeRolled
            , test "only pending dice can be rolled" onlyPendingDiceCanBeRolled
            , test "dice can not be rolled twice" diceCantBeRolledTwice
            ]
        , describe "direct outcomes"
            [ test "up to 7 is short-term fallout" upToSevenIsShortTermFallout
            , test "up to 11 is long-term fallout" upToElevenIsLongTermFallout
            , test "20 is imminent death" twentyIsImminentDeath
            ]
        , describe "avoiding medical attention"
            [ test "up to 15 is avoidable medical attention" upToFifteenIsAvoidableMedicalAttention
            , test "seeing with 3 dice is avoided medical attention" seeingWithThreeDiceIsAvoidedMedicalAttention
            , test "seeing with 4 dice is required medical attention" seeingWithFourDiceIsRequiredMedicalAttention
            ]
        ]


seeingWithFourDiceIsRequiredMedicalAttention : () -> Expectation
seeingWithFourDiceIsRequiredMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 8, Die.cheat D10 6 ]
                |> List.foldl Dice.add Dice.empty

        rolledBodyDice =
            [ Die.cheat D6 5, Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> List.foldl Dice.add Dice.empty
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.andThen (Fallout.roll rolledFalloutDice)
        |> Result.andThen (Fallout.rollPatientBody rolledBodyDice)
        |> Result.map Fallout.state
        |> Result.map
            (\state ->
                case state of
                    ExpectingDice _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "expected to be expecting conflict dice"
            )
        |> Result.withDefault (Expect.fail "should be ok")


seeingWithThreeDiceIsAvoidedMedicalAttention : () -> Expectation
seeingWithThreeDiceIsAvoidedMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 3, Die.cheat D10 6 ]
                |> List.foldl Dice.add Dice.empty

        rolledBodyDice =
            [ Die.cheat D6 5, Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> List.foldl Dice.add Dice.empty
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.andThen (Fallout.roll rolledFalloutDice)
        |> Result.andThen (Fallout.rollPatientBody rolledBodyDice)
        |> Result.map Fallout.state
        |> Result.map (Expect.equal (Concluded False DoubleLongTerm))
        |> Result.withDefault (Expect.fail "should be ok")


upToFifteenIsAvoidableMedicalAttention : () -> Expectation
upToFifteenIsAvoidableMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 3, Die.cheat D10 6 ]
                |> List.foldl Dice.add Dice.empty
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.andThen (Fallout.roll rolledFalloutDice)
        |> Result.map Fallout.state
        |> Result.map (Expect.equal (ExpectingPatientBody rolledFalloutDice))
        |> Result.withDefault (Expect.fail "should be ok")


twentyIsImminentDeath : () -> Expectation
twentyIsImminentDeath () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.andThen
            ([ Die.cheat D10 10, Die.cheat D10 10, Die.cheat D10 6 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Result.map Fallout.state
        |> Result.map (Expect.equal (Concluded False Dying))
        |> Result.withDefault (Expect.fail "should be ok")


upToElevenIsLongTermFallout : () -> Expectation
upToElevenIsLongTermFallout () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D6 Pips.three))
        |> Result.andThen
            ([ Die.cheat D6 6, Die.cheat D6 3, Die.cheat D6 2 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Result.map Fallout.state
        |> Result.map (Expect.equal (Concluded False LongTerm))
        |> Result.withDefault (Expect.fail "should be ok")


upToSevenIsShortTermFallout : () -> Expectation
upToSevenIsShortTermFallout () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen
            ([ Die.cheat D4 3, Die.cheat D4 3, Die.cheat D4 3 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Result.map Fallout.state
        |> Result.map (Expect.equal (Concluded False ShortTerm))
        |> Result.withDefault (Expect.fail "should be ok")


diceCantBeRolledTwice : () -> Expectation
diceCantBeRolledTwice () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen
            ([ Die.cheat D4 1, Die.cheat D4 3, Die.cheat D4 4 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Result.andThen
            ([ Die.cheat D4 2, Die.cheat D4 2, Die.cheat D4 4 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Expect.err


onlyPendingDiceCanBeRolled : () -> Expectation
onlyPendingDiceCanBeRolled () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen
            ([ Die.cheat D6 1, Die.cheat D8 3, Die.cheat D4 4 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Expect.err


diceCantBeTakenAfterRoll : () -> Expectation
diceCantBeTakenAfterRoll () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen
            ([ Die.cheat D4 1, Die.cheat D4 3, Die.cheat D4 4 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Result.andThen (Fallout.takeDice (Dice.init D6 Pips.three))
        |> Expect.err


pendingDiceCanBeRolled : () -> Expectation
pendingDiceCanBeRolled () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen
            ([ Die.cheat D4 1, Die.cheat D4 3, Die.cheat D4 4 ]
                |> List.foldl Dice.add Dice.empty
                |> Fallout.roll
            )
        |> Expect.ok


diceShouldBePiledTogether : () -> Expectation
diceShouldBePiledTogether () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen (Fallout.takeDice (Dice.init D6 Pips.four))
        |> Result.map Fallout.state
        |> Result.map
            (\state ->
                case state of
                    Pending dice ->
                        Dice.sizes dice
                            |> Expect.equalLists
                                [ D6, D6, D6, D6, D4, D4, D4 ]

                    _ ->
                        Expect.fail "expected Pending"
            )
        |> Result.withDefault (Expect.fail "expected Ok")
