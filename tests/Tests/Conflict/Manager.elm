module Tests.Conflict.Manager exposing (suite)

import Conflict.Manager as Manager
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Conflict manager"
        [ test "stores a string ID" storesStringId
        ]


storesStringId : () -> Expectation
storesStringId () =
    Manager.init "testingId"
        |> Manager.id
        |> Expect.equal "testingId"
