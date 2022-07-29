module Tests.Conflict.Manager exposing (suite)

import Conflict
import Conflict.Manager as Manager
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


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
        |> Expect.equal (Just Manager.SideAlreadyRegistered)


keepsErrorForLastUpdate : () -> Expectation
keepsErrorForLastUpdate () =
    Manager.init "testingId"
        |> Manager.register Conflict.proponent "proponent"
        |> Manager.register Conflict.opponent "proponent"
        |> Manager.error
        |> Expect.equal (Just Manager.CanNotParticipateAsBothSides)


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
