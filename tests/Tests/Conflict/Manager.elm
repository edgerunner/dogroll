module Tests.Conflict.Manager exposing (suite)

import Conflict
import Conflict.Manager as Manager exposing (Effect(..), Manager)
import Dice exposing (Dice)
import Die exposing (Rolled)
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Set
import Test exposing (Test, describe, test)
import Tests.Fuzzer as Fuzzer
import Tests.Helpers as Helpers


suite : Test
suite =
    describe "Conflict manager"
        [ test "stores a string ID" storesStringId
        , test "initializes and exposes a conflict" initializesAndExposesConflict
        , describe "registering"
            [ test "registers a participant as the proponent" registersProponent
            , test "does not register a participant if that side is already registered" doesNotRegisterIfAlreadyRegistered
            , test "does not register the same participant for both sides" doesNotRegisterSameParticipantForBothSides
            ]
        , describe "errors"
            [ test "sends the error for the last update" sendsErrorForLastUpdate
            , test "does not repeat previous errors" doesNotRepeatPreviousErrors
            ]
        , describe "actions"
            [ Test.fuzz2
                (Fuzzer.rolledDice Fuzzer.combinedDice)
                (Fuzzer.rolledDice Fuzzer.combinedDice)
                "passes actions to the conflict"
                passesActionsToConflict
            , Test.fuzz2
                (Fuzzer.rolledDice Fuzzer.combinedDice)
                (Fuzzer.rolledDice Fuzzer.combinedDice)
                "exposes conflict errors"
                exposesConflictErrors
            ]
        , describe "spectators"
            [ test "adds a spectator" addsASpectator
            , test "presents a subscriber list of participants and spectators" presentsNotificationList
            ]
        , describe "effects"
            [ Test.fuzz (Fuzzer.rolledDice Fuzzer.combinedDice)
                "sends the conflict update effect after successful actions"
                sendsConflictUpdateEffectAfterSuccessfulActions
            ]
        ]


sendsConflictUpdateEffectAfterSuccessfulActions : Dice Rolled -> Expectation
sendsConflictUpdateEffectAfterSuccessfulActions dice =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "opponent"
        , Manager.takeAction (Conflict.takeDice dice) "proponent"
        ]
        |> Tuple.second
        |> List.filterMap
            (\effect ->
                case effect of
                    StateUpdate recipents state ->
                        Helpers.allPass
                            [ recipents |> List.member "proponent" |> Expect.true "proponent is not in recipents"
                            , recipents |> List.member "opponent" |> Expect.true "opponent is not in recipents"
                            , state.proponent.pool |> Expect.equal dice
                            ]
                            |> Just

                    _ ->
                        Nothing
            )
        |> Helpers.allPass


presentsNotificationList : () -> Expectation
presentsNotificationList () =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "opponent"
        , Manager.addSpectator "spectator a"
        , Manager.addSpectator "spectator b"
        ]
        |> Tuple.first
        |> Manager.subscribers
        |> Expect.all
            ([ "proponent", "opponent", "spectator a", "spectator b" ]
                |> List.map (\id -> List.member id >> Expect.true (id ++ " not in list"))
            )


addsASpectator : () -> Expectation
addsASpectator () =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "opponent"
        , Manager.addSpectator "spectator"
        ]
        |> Tuple.first
        |> Manager.spectators
        |> Set.member "spectator"
        |> Expect.true "Expected to find spectator in spectators"


exposesConflictErrors : Dice Rolled -> Dice Rolled -> Expectation
exposesConflictErrors proponentDice opponentDice =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "opponent"
        , Manager.takeAction (Conflict.takeDice proponentDice) "proponent"
        , Manager.takeAction (Conflict.takeDice opponentDice) "opponent"

        -- Deliberately play impossible die to test that the error is exposed.
        , Manager.takeAction (Conflict.play (Die.cheat D4 10)) "proponent"
        ]
        |> Tuple.second
        |> Expect.equal [ Manager.ErrorResponse "proponent" (Manager.ConflictError Conflict.DieNotInPool) ]


passesActionsToConflict : Dice Rolled -> Dice Rolled -> Expectation
passesActionsToConflict proponentDice opponentDice =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "opponent"
        , Manager.takeAction (Conflict.takeDice proponentDice) "proponent"
        , Manager.takeAction (Conflict.takeDice opponentDice) "opponent"
        ]
        |> Tuple.first
        |> Manager.conflict
        |> Conflict.state
        |> Expect.all
            [ .proponent >> .pool >> Expect.equal proponentDice
            , .opponent >> .pool >> Expect.equal opponentDice
            ]


doesNotRepeatPreviousErrors : () -> Expectation
doesNotRepeatPreviousErrors () =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "proponent"
        , Manager.register Conflict.proponent "someone else"
        , Manager.register Conflict.opponent "opponent"
        ]
        |> Expect.all
            [ Tuple.first
                >> Manager.proponent
                >> Maybe.map .id
                >> Expect.equal (Just "proponent")
            , Tuple.first
                >> Manager.opponent
                >> Maybe.map .id
                >> Expect.equal (Just "opponent")
            , Tuple.second
                >> List.member (Manager.ErrorResponse "someone else" Manager.SideAlreadyRegistered)
                >> Expect.false "Expected to find no errors"
            ]


sendsErrorForLastUpdate : () -> Expectation
sendsErrorForLastUpdate () =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "proponent"
        ]
        |> Tuple.second
        |> Expect.equal [ Manager.ErrorResponse "proponent" Manager.CanNotParticipateAsBothSides ]


doesNotRegisterSameParticipantForBothSides : () -> Expectation
doesNotRegisterSameParticipantForBothSides () =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.opponent "proponent"
        ]
        |> Tuple.first
        |> Manager.opponent
        |> Maybe.map .id
        |> Expect.equal Nothing


doesNotRegisterIfAlreadyRegistered : () -> Expectation
doesNotRegisterIfAlreadyRegistered () =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent"
        , Manager.register Conflict.proponent "someone else"
        ]
        |> Tuple.first
        |> Manager.proponent
        |> Maybe.map .id
        |> Expect.equal (Just "proponent")


registersProponent : () -> Expectation
registersProponent () =
    run (Manager.init "testingId")
        [ Manager.register Conflict.proponent "proponent" ]
        |> Tuple.first
        |> Manager.proponent
        |> Maybe.map .id
        |> Expect.equal (Just "proponent")


initializesAndExposesConflict : () -> Expectation
initializesAndExposesConflict () =
    Manager.init "testingId"
        |> Manager.conflict
        |> Expect.equal Conflict.start


storesStringId : () -> Expectation
storesStringId () =
    Manager.init "testingId"
        |> Manager.id
        |> Expect.equal "testingId"



-- HELPERS


{-| This is a helper function that runs the `Manager` with the given actions.
It ignores the effects fired in between,
returning the result of the last action.
-}
run : Manager -> List (Manager -> ( Manager, List Effect )) -> ( Manager, List Effect )
run initialManager =
    List.foldl
        (\action -> Tuple.first >> action)
        ( initialManager, [] )
