module Tests.Dice exposing (suite)

import Dice
import Die
import Die.Size
import Expect
import Test exposing (Test, describe, fuzz, test)
import Tests.Helpers as Helpers


seed : Int
seed =
    42


suite : Test
suite =
    describe "Dice"
        [ test "initializing multiple dice"
            (\_ ->
                Dice.init seed 4 (Die.init Die.Size.D6)
                    |> Dice.faces
                    |> List.map (Helpers.between1and 6)
                    |> Helpers.allPass
            )
        , test "combining dice"
            (\_ ->
                Dice.combine
                    [ Dice.init seed 2 (Die.init Die.Size.D8)
                    , Dice.init seed 1 (Die.init Die.Size.D4)
                    ]
                    |> Dice.sizes
                    |> Expect.equalLists [ Die.Size.D8, Die.Size.D8, Die.Size.D4 ]
            )
        , fuzz Helpers.diceFuzzer
            "rolling all dice at once"
            (Dice.roll
                >> Dice.toList
                >> List.map
                    (\die ->
                        Helpers.between1and
                            (Die.size die |> Die.Size.toInt)
                            (Die.face die)
                    )
                >> Helpers.allPass
            )
        , fuzz Helpers.combinedDiceFuzzer
            "are always sorted"
            (\pool ->
                let
                    faces =
                        Dice.faces pool

                    sorted =
                        faces |> List.sort |> List.reverse

                    dieByDie =
                        pool
                            |> Dice.toList
                            |> List.map Die.face
                in
                faces
                    |> Expect.all
                        [ Expect.equalLists sorted
                        , Expect.equalLists dieByDie
                        ]
            )
        , describe
            "string representation"
            [ test "single type"
                (\_ ->
                    Dice.init seed 2 (Die.init Die.Size.D4)
                        |> Dice.toString
                        |> Expect.equal "2d4"
                )
            , test "one of single type"
                (\_ ->
                    Dice.init seed 1 (Die.init Die.Size.D10)
                        |> Dice.toString
                        |> Expect.equal "1d10"
                )
            , test "multiple types"
                (\_ ->
                    Dice.combine
                        [ Dice.init seed 1 (Die.init Die.Size.D4)
                        , Dice.init seed 2 (Die.init Die.Size.D8)
                        ]
                        |> Dice.toString
                        |> Expect.equal "2d8+1d4"
                )
            ]
        ]
