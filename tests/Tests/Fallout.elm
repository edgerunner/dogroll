module Tests.Fallout exposing (suite)

import Conflict
import Dice
import Die
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Fallout exposing (ConflictDice, Fallout, Outcome(..), State(..))
import Pips
import Random exposing (Seed)
import Test exposing (Test, describe, test)
import Tests.Fuzzer


suite : Test
suite =
    describe "Fallout"
        [ describe "taking dice"
            [ test "dice should be piled together" diceShouldBePiledTogether
            , test "dice can't be taken after a roll" diceCantBeTakenAfterRoll
            ]
        , describe "rolling dice"
            [ test "pending dice can be rolled" pendingDiceCanBeRolled
            , test "dice can not be rolled twice" diceCantBeRolledTwice
            ]
        , describe "direct outcomes"
            [ test "up to 7 is short-term fallout" upToSevenIsShortTermFallout
            , test "up to 11 is long-term fallout" upToElevenIsLongTermFallout
            , test "20 is imminent death" twentyIsImminentDeath
            ]
        , describe "avoiding medical attention"
            [ test "up to 15 is avoidable medical attention"
                upToFifteenIsAvoidableMedicalAttention
            , test "seeing with 3 dice is avoided medical attention"
                seeingWithThreeDiceIsAvoidedMedicalAttention
            , test "seeing with 4 dice is required medical attention"
                seeingWithFourDiceIsRequiredMedicalAttention
            , test "medical attention is required if the patient can not see"
                medicalAttentionIsRequiredIfThePatientCanNotSee
            ]
        , describe "required medical attention"
            [ test "up to 19 is required medical attention"
                upToNineteenIsRequiredMedicalAttention
            , test "failed avoid roll is required medical attention"
                failedAvoidRollIsRequiredMedicalAttention
            , test "can take body dice if not taken previously"
                canTakeBodyDiceIfNotTakenPreviously
            , test "body dice are automatically taken after a failed avoid roll"
                bodyDiceAreAutomaticallyTakenAfterAFailedAvoidRoll
            , test "body dice can not be manually taken after a previous body roll"
                bodyDiceCanNotBeManuallyTakenAfterAPreviousBodyRoll
            , test "dice can be taken" diceCanBeTaken
            , test "dice can't be taken unless expected" diceCantBeTakenUnlessExpected
            ]
        , describe "starting a conflict"
            [ test "all conflict dice must be taken" allConflictDiceMustBeTaken
            , Test.fuzz Tests.Fuzzer.seed
                "conflict starts with matching dice"
                conflictStartsWithMatchingDice
            ]
        ]


conflictStartsWithMatchingDice : Seed -> Expectation
conflictStartsWithMatchingDice seed =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice <| Dice.init D6 Pips.two)
        |> Result.andThen (Fallout.takeHealerAcuityDice <| Dice.init D6 Pips.three)
        |> Result.andThen (Fallout.takeDemonicInfluenceDice <| Dice.init D10 Pips.one)
        |> Result.andThen Fallout.startConflict
        |> Result.map (Random.step >> (|>) seed >> Tuple.first)
        |> Result.map Fallout.state
        |> (\stateResult ->
                case stateResult of
                    Ok (InConflict conflict) ->
                        Conflict.state conflict
                            |> Expect.all
                                [ .proponent
                                    >> .pool
                                    >> Dice.hold
                                    >> Expect.equal (Dice.init D6 Pips.five)
                                , .opponent
                                    >> .pool
                                    >> Dice.hold
                                    >> Expect.equal (Dice.init D10 Pips.four)
                                ]

                    _ ->
                        Expect.fail "state is not in conflict"
           )


allConflictDiceMustBeTaken : () -> Expectation
allConflictDiceMustBeTaken () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice <| Dice.init D6 Pips.two)
        |> Result.andThen (Fallout.takeHealerAcuityDice <| Dice.init D6 Pips.three)
        |> Expect.all
            [ Result.andThen Fallout.startConflict >> Expect.err
            , Result.andThen (Fallout.takeDemonicInfluenceDice <| Dice.init D10 Pips.one)
                >> Result.andThen Fallout.startConflict
                >> Expect.ok
            ]


diceCantBeTakenUnlessExpected : () -> Expectation
diceCantBeTakenUnlessExpected () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Expect.all
            [ Result.andThen (Fallout.takePatientBodyDice <| Dice.init D6 Pips.three) >> Expect.err
            , Result.andThen (Fallout.takeHealerAcuityDice <| Dice.init D6 Pips.three) >> Expect.err
            , Result.andThen (Fallout.takeDemonicInfluenceDice <| Dice.init D10 Pips.three) >> Expect.err
            ]


diceCanBeTaken : () -> Expectation
diceCanBeTaken () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice <| Dice.init D6 Pips.two)
        |> Result.andThen (Fallout.takeHealerAcuityDice <| Dice.init D6 Pips.three)
        |> Result.andThen (Fallout.takeDemonicInfluenceDice <| Dice.init D10 Pips.one)
        |> expectStateExpectingDiceWith
            (Expect.all
                [ .fallout >> Expect.equal (Dice.init D10 Pips.three)
                , .patientBody >> Expect.equal (Dice.init D6 Pips.two |> Just)
                , .healerAcuity >> Expect.equal (Dice.init D6 Pips.three |> Just)
                , .demonicInfluence >> Expect.equal (Dice.init D10 Pips.one |> Just)
                ]
            )


bodyDiceCanNotBeManuallyTakenAfterAPreviousBodyRoll : () -> Expectation
bodyDiceCanNotBeManuallyTakenAfterAPreviousBodyRoll () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 8, Die.cheat D10 6, Die.cheat D10 3 ]
                |> Dice.fromList

        rolledBodyDice =
            [ Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.four))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> Result.andThen (Fallout.takePatientBodyDice <| Dice.init D6 Pips.two)
        |> Expect.err


bodyDiceAreAutomaticallyTakenAfterAFailedAvoidRoll : () -> Expectation
bodyDiceAreAutomaticallyTakenAfterAFailedAvoidRoll () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 8, Die.cheat D10 6, Die.cheat D10 3 ]
                |> Dice.fromList

        rolledBodyDice =
            [ Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.four))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> expectStateExpectingDiceWith
            (.patientBody >> Expect.equal (Just <| Dice.init D6 Pips.three))


canTakeBodyDiceIfNotTakenPreviously : () -> Expectation
canTakeBodyDiceIfNotTakenPreviously () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice <| Dice.init D6 Pips.two)
        |> expectStateExpectingDiceWith
            (Expect.all
                [ .fallout >> Expect.equal (Dice.init D10 Pips.three)
                , .patientBody >> Expect.equal (Dice.init D6 Pips.two |> Just)
                ]
            )


failedAvoidRollIsRequiredMedicalAttention : () -> Expectation
failedAvoidRollIsRequiredMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 8, Die.cheat D10 6, Die.cheat D10 3 ]
                |> Dice.fromList

        rolledBodyDice =
            [ Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.four))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> expectStateExpectingDiceWith
            (Expect.all
                [ .fallout >> Expect.equal (Dice.init D10 Pips.four)
                , .patientBody >> Expect.equal (Dice.init D6 Pips.three |> Just)
                , .healerAcuity >> Expect.equal Nothing
                , .demonicInfluence >> Expect.equal Nothing
                ]
            )


upToNineteenIsRequiredMedicalAttention : () -> Expectation
upToNineteenIsRequiredMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> expectStateExpectingDiceWith
            (.fallout >> Expect.equal (Dice.init D10 Pips.three))


medicalAttentionIsRequiredIfThePatientCanNotSee : () -> Expectation
medicalAttentionIsRequiredIfThePatientCanNotSee () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 8, Die.cheat D10 6 ]
                |> Dice.fromList

        rolledBodyDice =
            [ Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> expectStateExpectingDiceWith (always Expect.pass)


seeingWithFourDiceIsRequiredMedicalAttention : () -> Expectation
seeingWithFourDiceIsRequiredMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 8, Die.cheat D10 6 ]
                |> Dice.fromList

        rolledBodyDice =
            [ Die.cheat D6 5, Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> expectStateExpectingDiceWith (always Expect.pass)


seeingWithThreeDiceIsAvoidedMedicalAttention : () -> Expectation
seeingWithThreeDiceIsAvoidedMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 3, Die.cheat D10 6 ]
                |> Dice.fromList

        rolledBodyDice =
            [ Die.cheat D6 5, Die.cheat D6 4, Die.cheat D6 4, Die.cheat D6 2 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> expectState (Concluded False DoubleLongTerm)


upToFifteenIsAvoidableMedicalAttention : () -> Expectation
upToFifteenIsAvoidableMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 3, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> expectState (ExpectingPatientBody rolledFalloutDice)


twentyIsImminentDeath : () -> Expectation
twentyIsImminentDeath () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D10 Pips.three))
        |> Result.map
            ([ Die.cheat D10 10, Die.cheat D10 10, Die.cheat D10 6 ]
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> expectState (Concluded False Dying)


upToElevenIsLongTermFallout : () -> Expectation
upToElevenIsLongTermFallout () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D6 Pips.three))
        |> Result.map
            ([ Die.cheat D6 6, Die.cheat D6 3, Die.cheat D6 2 ]
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> expectState (Concluded False LongTerm)


upToSevenIsShortTermFallout : () -> Expectation
upToSevenIsShortTermFallout () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.map
            ([ Die.cheat D4 3, Die.cheat D4 3, Die.cheat D4 3 ]
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> expectState (Concluded False ShortTerm)


diceCantBeRolledTwice : () -> Expectation
diceCantBeRolledTwice () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen Fallout.roll
        |> Result.map (Random.step >> (|>) (Random.initialSeed 0) >> Tuple.first)
        |> Result.andThen Fallout.roll
        |> Expect.err


diceCantBeTakenAfterRoll : () -> Expectation
diceCantBeTakenAfterRoll () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen Fallout.roll
        |> Result.map (Random.step >> (|>) (Random.initialSeed 0) >> Tuple.first)
        |> Result.andThen (Fallout.takeDice (Dice.init D6 Pips.three))
        |> Expect.err


pendingDiceCanBeRolled : () -> Expectation
pendingDiceCanBeRolled () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen Fallout.roll
        |> Result.map (Random.step >> (|>) (Random.initialSeed 0) >> Tuple.first)
        |> Result.map Fallout.state
        |> (\resultingState ->
                case resultingState of
                    Err _ ->
                        Expect.fail "Should be able to roll without errors"

                    Ok (Pending _) ->
                        Expect.fail "Should not stay in pending state"

                    Ok (InConflict _) ->
                        Expect.fail "Should not go to conflict state right away"

                    Ok _ ->
                        Expect.pass
           )


diceShouldBePiledTogether : () -> Expectation
diceShouldBePiledTogether () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen (Fallout.takeDice (Dice.init D6 Pips.four))
        |> expectStateWith
            (\state ->
                case state of
                    Pending dice ->
                        Dice.sizes dice
                            |> Expect.equalLists
                                [ D6, D6, D6, D6, D4, D4, D4 ]

                    _ ->
                        Expect.fail "expected Pending"
            )



-- HELPERS


expectState : State -> Result error Fallout -> Expectation
expectState expectedState =
    Expect.equal expectedState |> expectStateWith


expectStateWith : (State -> Expectation) -> Result error Fallout -> Expectation
expectStateWith stateToExpectation =
    Result.map Fallout.state
        >> Result.map stateToExpectation
        >> Result.withDefault (Expect.fail "expected Ok")


expectStateExpectingDiceWith : (ConflictDice -> Expectation) -> Result error Fallout -> Expectation
expectStateExpectingDiceWith diceToExpectaion =
    expectStateWith
        (\state ->
            case state of
                ExpectingDice dice ->
                    diceToExpectaion dice

                _ ->
                    Expect.fail "expected to be expecting conflict dice"
        )
