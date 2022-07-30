module Tests.Conflict.Manager exposing (suite)

import Conflict
import Conflict.Manager as Manager
import Dice exposing (Dice)
import Die exposing (Rolled)
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Set
import Test exposing (Test, describe, test)
import Tests.Fuzzer as Fuzzer


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
            [ test "keeps the error for the last update" keepsErrorForLastUpdate
            , test "replaces the error with the last update" replacesErrorWithLastUpdate
            , test "clears the error after a successful update" clearsErrorAfterSuccessfulUpdate
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
        ]


presentsNotificationList : () -> Expectation
presentsNotificationList () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "opponent"
        |> Manager.addSpectator "spectator a"
        |> Manager.addSpectator "spectator b"
        |> Manager.subscribers
        |> Expect.all
            ([ "proponent", "opponent", "spectator a", "spectator b" ]
                |> List.map (\id -> List.member id >> Expect.true (id ++ " not in list"))
            )


addsASpectator : () -> Expectation
addsASpectator () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "opponent"
        |> Manager.addSpectator "spectator"
        |> Manager.spectators
        |> Set.member "spectator"
        |> Expect.true "Expected to find spectator in spectators"


exposesConflictErrors : Dice Rolled -> Dice Rolled -> Expectation
exposesConflictErrors proponentDice opponentDice =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "opponent"
        |> Manager.takeAction (Conflict.takeDice proponentDice) "proponent"
        |> Manager.takeAction (Conflict.takeDice opponentDice) "opponent"
        -- Deliberately play impossible die to test that the error is exposed.
        |> Manager.takeAction (Conflict.play (Die.cheat D4 10)) "proponent"
        |> Manager.error
        |> Expect.equal (Just ( Manager.ConflictError Conflict.DieNotInPool, "proponent" ))


passesActionsToConflict : Dice Rolled -> Dice Rolled -> Expectation
passesActionsToConflict proponentDice opponentDice =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "opponent"
        |> Manager.takeAction (Conflict.takeDice proponentDice) "proponent"
        |> Manager.takeAction (Conflict.takeDice opponentDice) "opponent"
        |> Manager.conflict
        |> Conflict.state
        |> Expect.all
            [ .proponent >> .pool >> Expect.equal proponentDice
            , .opponent >> .pool >> Expect.equal opponentDice
            ]


clearsErrorAfterSuccessfulUpdate : () -> Expectation
clearsErrorAfterSuccessfulUpdate () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "proponent"
        |> Manager.register Conflict.proponent "someone else"
        |> Manager.register Conflict.opponent "opponent"
        |> Expect.all
            [ Manager.error >> Expect.equal Nothing
            , Manager.proponent >> Maybe.map .id >> Expect.equal (Just "proponent")
            , Manager.opponent >> Maybe.map .id >> Expect.equal (Just "opponent")
            ]


replacesErrorWithLastUpdate : () -> Expectation
replacesErrorWithLastUpdate () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "proponent"
        |> Manager.register Conflict.proponent "someone else"
        |> Manager.error
        |> Expect.equal (Just ( Manager.SideAlreadyRegistered, "someone else" ))


keepsErrorForLastUpdate : () -> Expectation
keepsErrorForLastUpdate () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "proponent"
        |> Manager.error
        |> Expect.equal (Just ( Manager.CanNotParticipateAsBothSides, "proponent" ))


doesNotRegisterSameParticipantForBothSides : () -> Expectation
doesNotRegisterSameParticipantForBothSides () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "proponent"
        |> Manager.opponent
        |> Maybe.map .id
        |> Expect.equal Nothing


doesNotRegisterIfAlreadyRegistered : () -> Expectation
doesNotRegisterIfAlreadyRegistered () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.proponent "someone else"
        |> Manager.proponent
        |> Maybe.map .id
        |> Expect.equal (Just "proponent")


registersProponent : () -> Expectation
registersProponent () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
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
