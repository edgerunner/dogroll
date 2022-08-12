module Tests.Fallout exposing (suite)

import Dice
import Die.Size exposing (Size(..))
import Expect exposing (Expectation)
import Fallout exposing (State(..))
import Pips
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Fallout"
        [ describe "taking dice"
            [ test "dice should be piled together" diceShouldBePiledTogether ]
        ]


diceShouldBePiledTogether : () -> Expectation
diceShouldBePiledTogether () =
    Ok Fallout.init
        |> Result.andThen (Fallout.takeDice (Dice.init D4 Pips.three))
        |> Result.andThen (Fallout.takeDice (Dice.init D6 Pips.four))
        |> Result.map Fallout.state
        |> Result.map
            (\state ->
                case state of
                    Pending dice ->
                        Dice.sizes dice
                            |> Expect.equalLists
                                [ D6, D6, D6, D6, D4, D4, D4 ]

                    _ ->
                        Expect.fail "expected Pending"
            )
        |> Result.withDefault (Expect.fail "expected Ok")
