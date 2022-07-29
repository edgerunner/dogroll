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
        ]


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
