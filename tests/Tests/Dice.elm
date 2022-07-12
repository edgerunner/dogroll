module Tests.Dice exposing (suite)

import Dice
import Die
import Expect
import Test exposing (Test, describe, fuzz, test, todo)
import Tests.Helpers as Helpers


seed : Int
seed =
    42


suite : Test
suite =
    describe "Dice"
        [ test "initializing multiple dice"
            (\_ ->
                Dice.init seed 4 Die.d6
                    |> Dice.faces
                    |> List.map (Helpers.between1and 6)
                    |> Helpers.allPass
            )
        , test "combining dice"
            (\_ ->
                Dice.combine
                    [ Dice.init seed 2 Die.d8
                    , Dice.init seed 1 Die.d4
                    ]
                    |> Dice.sizes
                    |> Expect.equalLists [ Die.D8, Die.D8, Die.D4 ]
            )
        , fuzz Helpers.diceFuzzer
            "rolling all dice at once"
            (Dice.roll
                >> Dice.toList
                >> List.map
                    (\die ->
                        Helpers.between1and
                            (Die.faces die)
                            (Die.face die)
                    )
                >> Helpers.allPass
            )
        , todo "getting the best raise"
        ]
