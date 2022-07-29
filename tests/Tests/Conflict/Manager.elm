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
        , test "registers a participant as the proponent" registersProponent
        , test "does not register a participant if that side is already registered" doesNotRegisterIfAlreadyRegistered
        ]


doesNotRegisterIfAlreadyRegistered : () -> Expectation
doesNotRegisterIfAlreadyRegistered () =
    Manager.init "for double proponent"
        |> Manager.registerProponent "proponent"
        |> Manager.registerProponent "someone else"
        |> Manager.proponent
        |> Maybe.map .id
        |> Expect.equal (Just "proponent")


registersProponent : () -> Expectation
registersProponent () =
    Manager.init "for proponent"
        |> Manager.registerProponent "proponent"
        |> Manager.proponent
        |> Maybe.map .id
        |> Expect.equal (Just "proponent")


initializesAndExposesConflict : () -> Expectation
initializesAndExposesConflict () =
    Manager.init "foo"
        |> Manager.conflict
        |> Expect.equal Conflict.start


storesStringId : () -> Expectation
storesStringId () =
    Manager.init "testingId"
        |> Manager.id
        |> Expect.equal "testingId"
