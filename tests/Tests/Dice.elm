module Tests.Dice exposing (suite)

import Dice
import Die
import Test exposing (Test, describe, test, todo)
import Tests.Helpers as Helpers


seed : Int
seed =
    42


suite : Test
suite =
    describe "Dice"
        [ test "initializing multiple dice"
            (\() ->
                Dice.init seed 4 Die.d6
                    |> Dice.faces
                    |> List.map (Helpers.between1and 6)
                    |> Helpers.allPass
            )
        , todo "combining dice"
        , todo "rolling all dice at once"
        , todo "getting the best raise"
        ]
