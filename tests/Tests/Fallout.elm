module Tests.Fallout exposing (suite)

import Conflict
import Dice
import Die
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Fallout exposing (ConflictDice, Experience(..), Fallout, Outcome(..), State(..))
import Pips
import Random exposing (Generator, Seed)
import Test exposing (Test, describe, test)
import Tests.Fuzzer


suite : Test
suite =
    describe "Fallout"
        [ describe "initial state"
            [ test "is pending if there are dice" initialStateIsPending
            , test "having no dice is an error" noDiceIsError
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
        , describe "ending a conflict"
            [ test "a matching conflict can be used to determine fallout"
                matchingConflictCanBeUsedToDetermineFallout
            , test "a non-matching conflict is an error"
                nonMatchingConflictIsAnError
            , test "an ongoing conflict is an error" ongoingConflictIsAnError
            , test "outcome is death if the proponent (healer) gives"
                outcomeIsDeathIfTheProponentGives
            , test "outcome is injury if the opponent (GM) gives"
                outcomeIsInjuryIfTheOpponentGives
            ]
        , describe "experience"
            [ test "status is indeterminate before fallout is rolled"
                statusIsIndeterminateBeforeFalloutIsRolled
            , test "a 1 on the fallout dice is experience"
                aOneOnTheFalloutDiceIsExperience
            , test "no 1's on the fallout dice is no experience"
                noOnesOnTheFalloutDiceIsNoExperience
            ]
        ]


noOnesOnTheFalloutDiceIsNoExperience : () -> Expectation
noOnesOnTheFalloutDiceIsNoExperience () =
    Dice.init D4 Pips.four
        |> Fallout.init
        |> Result.map
            ([ 2, 3, 2, 4 ]
                |> List.map (Die.cheat D4)
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> Result.map Fallout.experience
        |> Result.map (Expect.equal NoExperience)
        |> Result.withDefault (Expect.fail "did not expect an error")


aOneOnTheFalloutDiceIsExperience : () -> Expectation
aOneOnTheFalloutDiceIsExperience () =
    Dice.init D4 Pips.four
        |> Fallout.init
        |> Result.map
            ([ 2, 1, 1, 4 ]
                |> List.map (Die.cheat D4)
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> Result.map Fallout.experience
        |> Result.map (Expect.equal <| Experience (Die.cheat D4 1))
        |> Result.withDefault (Expect.fail "did not expect an error")


statusIsIndeterminateBeforeFalloutIsRolled : () -> Expectation
statusIsIndeterminateBeforeFalloutIsRolled () =
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map Fallout.experience
        |> Result.map (Expect.equal Indeterminate)
        |> Result.withDefault (Expect.fail "did not expect an error")


falloutSetupForConflict : Result Fallout.Error Fallout
falloutSetupForConflict =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList

        proponentDice =
            [ 6, 4, 3, 3, 1 ] |> List.map (Die.cheat D6) |> Dice.fromList

        opponentDice =
            [ 8, 2, 9, 4 ] |> List.map (Die.cheat D10) |> Dice.fromList
    in
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice Pips.two)
        |> Result.andThen (Fallout.takeHealerAcuityDice Pips.three)
        |> Result.andThen (Fallout.takeDemonicInfluenceDice Pips.one)
        |> Result.map (Fallout.test_startConflict proponentDice opponentDice)


outcomeIsInjuryIfTheOpponentGives : () -> Expectation
outcomeIsInjuryIfTheOpponentGives () =
    let
        conflict =
            falloutSetupForConflict
                |> Result.map Fallout.state
                |> (\state ->
                        case state of
                            Ok (InConflict conflict_) ->
                                Ok conflict_

                            _ ->
                                Ok Conflict.start
                   )
                |> Result.andThen (Conflict.play (Die.cheat D6 4) Conflict.proponent)
                |> Result.andThen (Conflict.play (Die.cheat D6 6) Conflict.proponent)
                |> Result.andThen (Conflict.raise Conflict.proponent)
                |> Result.andThen (Conflict.give Conflict.opponent)
                |> Result.mapError (always Fallout.UnableToStartConflict)
    in
    falloutSetupForConflict
        |> Result.map2 Fallout.endConflict conflict
        |> Result.andThen identity
        |> expectState (Concluded DoubleLongTerm)


outcomeIsDeathIfTheProponentGives : () -> Expectation
outcomeIsDeathIfTheProponentGives () =
    let
        conflict =
            falloutSetupForConflict
                |> extractConflict
                |> Conflict.give Conflict.proponent
                |> Result.mapError (always Fallout.UnableToStartConflict)
    in
    falloutSetupForConflict
        |> Result.map2 Fallout.endConflict conflict
        |> Result.andThen identity
        |> expectState (Concluded Dying)


ongoingConflictIsAnError : () -> Expectation
ongoingConflictIsAnError () =
    let
        conflict =
            falloutSetupForConflict
                |> extractConflict
    in
    falloutSetupForConflict
        |> Result.map (Fallout.endConflict conflict)
        |> Result.andThen identity
        |> Expect.err


nonMatchingConflictIsAnError : () -> Expectation
nonMatchingConflictIsAnError () =
    let
        conflict =
            Conflict.start
                |> Conflict.give Conflict.proponent
                |> Result.mapError (always Fallout.UnableToStartConflict)
    in
    falloutSetupForConflict
        |> Result.map2 Fallout.endConflict conflict
        |> Result.andThen identity
        |> Expect.err


matchingConflictCanBeUsedToDetermineFallout : () -> Expectation
matchingConflictCanBeUsedToDetermineFallout () =
    let
        conflict =
            falloutSetupForConflict
                |> extractConflict
                |> Conflict.give Conflict.proponent
                |> Result.mapError (always Fallout.UnableToStartConflict)
    in
    falloutSetupForConflict
        |> Result.map2 Fallout.endConflict conflict
        |> Result.andThen identity
        |> Expect.ok


conflictStartsWithMatchingDice : Seed -> Expectation
conflictStartsWithMatchingDice seed =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice Pips.two)
        |> Result.andThen (Fallout.takeHealerAcuityDice Pips.three)
        |> Result.andThen (Fallout.takeDemonicInfluenceDice Pips.one)
        |> Result.andThen Fallout.startConflict
        |> Result.map (stepWith seed)
        |> expectStateWith
            (\state ->
                case state of
                    InConflict conflict ->
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
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice Pips.two)
        |> Result.andThen (Fallout.takeHealerAcuityDice Pips.three)
        |> Expect.all
            [ Result.andThen Fallout.startConflict >> Expect.err
            , Result.andThen (Fallout.takeDemonicInfluenceDice Pips.one)
                >> Result.andThen Fallout.startConflict
                >> Expect.ok
            ]


diceCantBeTakenUnlessExpected : () -> Expectation
diceCantBeTakenUnlessExpected () =
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Expect.all
            [ Result.andThen (Fallout.takePatientBodyDice Pips.three) >> Expect.err
            , Result.andThen (Fallout.takeHealerAcuityDice Pips.three) >> Expect.err
            , Result.andThen (Fallout.takeDemonicInfluenceDice Pips.three) >> Expect.err
            ]


diceCanBeTaken : () -> Expectation
diceCanBeTaken () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 10, Die.cheat D10 7, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice Pips.two)
        |> Result.andThen (Fallout.takeHealerAcuityDice Pips.three)
        |> Result.andThen (Fallout.takeDemonicInfluenceDice Pips.one)
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
    Dice.init D10 Pips.four
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> Result.andThen (Fallout.takePatientBodyDice Pips.two)
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
    Dice.init D10 Pips.four
        |> Fallout.init
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
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.andThen (Fallout.takePatientBodyDice Pips.two)
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
    Dice.init D10 Pips.three
        |> Fallout.init
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
    Dice.init D10 Pips.three
        |> Fallout.init
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
    Dice.init D10 Pips.three
        |> Fallout.init
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
    Dice.init D10 Pips.three
        |> Fallout.init
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
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> Result.map (Fallout.test_rollPatientBody rolledBodyDice)
        |> expectState (Concluded DoubleLongTerm)


upToFifteenIsAvoidableMedicalAttention : () -> Expectation
upToFifteenIsAvoidableMedicalAttention () =
    let
        rolledFalloutDice =
            [ Die.cheat D10 7, Die.cheat D10 3, Die.cheat D10 6 ]
                |> Dice.fromList
    in
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map (Fallout.test_roll rolledFalloutDice)
        |> expectState (ExpectingPatientBody rolledFalloutDice)


twentyIsImminentDeath : () -> Expectation
twentyIsImminentDeath () =
    Dice.init D10 Pips.three
        |> Fallout.init
        |> Result.map
            ([ Die.cheat D10 10, Die.cheat D10 10, Die.cheat D10 6 ]
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> expectState (Concluded Dying)


upToElevenIsLongTermFallout : () -> Expectation
upToElevenIsLongTermFallout () =
    Dice.init D6 Pips.three
        |> Fallout.init
        |> Result.map
            ([ Die.cheat D6 6, Die.cheat D6 3, Die.cheat D6 2 ]
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> expectState (Concluded LongTerm)


upToSevenIsShortTermFallout : () -> Expectation
upToSevenIsShortTermFallout () =
    Dice.init D4 Pips.three
        |> Fallout.init
        |> Result.map
            ([ Die.cheat D4 3, Die.cheat D4 3, Die.cheat D4 3 ]
                |> Dice.fromList
                |> Fallout.test_roll
            )
        |> expectState (Concluded ShortTerm)


diceCantBeRolledTwice : () -> Expectation
diceCantBeRolledTwice () =
    Dice.init D4 Pips.three
        |> Fallout.init
        |> Result.andThen Fallout.roll
        |> Result.map (stepWith <| Random.initialSeed 0)
        |> Result.andThen Fallout.roll
        |> Expect.err


pendingDiceCanBeRolled : () -> Expectation
pendingDiceCanBeRolled () =
    Dice.init D4 Pips.three
        |> Fallout.init
        |> Result.andThen Fallout.roll
        |> Result.map (stepWith <| Random.initialSeed 0)
        |> expectStateWith
            (\state ->
                case state of
                    Pending _ ->
                        Expect.fail "Should not stay in pending state"

                    InConflict _ ->
                        Expect.fail "Should not go to conflict state right away"

                    _ ->
                        Expect.pass
            )


noDiceIsError : () -> Expectation
noDiceIsError () =
    Dice.empty
        |> Fallout.init
        |> Expect.err


initialStateIsPending : () -> Expectation
initialStateIsPending () =
    [ Dice.init D4 Pips.three, Dice.init D6 Pips.four ]
        |> Dice.combine
        |> Fallout.init
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


stepWith : Seed -> Generator Fallout -> Fallout
stepWith seed gen =
    Random.step gen seed |> Tuple.first


extractConflict : Result Fallout.Error Fallout -> Conflict.Conflict
extractConflict =
    Result.map Fallout.state
        >> (\state ->
                case state of
                    Ok (InConflict conflict_) ->
                        conflict_

                    _ ->
                        Conflict.start
           )
