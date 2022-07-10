module Tests.Die exposing (..)

import Die exposing (Die)
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


dice : List (Int -> Die)
dice =
    [ Die.d4, Die.d6, Die.d8, Die.d10 ]


multipleDice : Fuzz.Fuzzer Die
multipleDice =
    Fuzz.map2 (|>) Fuzz.int (Fuzz.oneOf (List.map Fuzz.constant dice))


suite : Test
suite =
    describe "Die"
        [ test "reporting number of sides"
            (\() ->
                dice
                    |> List.map ((|>) 0)
                    |> List.map Die.sides
                    |> Expect.equal [ 4, 6, 8, 10 ]
            )
        , fuzz multipleDice
            "rolling"
            (Die.roll
                >> (\d -> ( d, d ))
                >> Tuple.mapBoth Die.sides Die.face
                >> (\( sides, side ) ->
                        side
                            |> Expect.all
                                [ Expect.atLeast 1
                                , Expect.atMost sides
                                ]
                   )
            )
        , fuzz multipleDice
            "starting with a rolled face"
            ((\d -> ( d, d ))
                >> Tuple.mapBoth Die.sides Die.face
                >> (\( sides, side ) ->
                        side
                            |> Expect.all
                                [ Expect.atLeast 1
                                , Expect.atMost sides
                                ]
                   )
            )
        , test "string representation"
            (\() ->
                dice
                    |> List.map ((|>) 0 >> Die.toString)
                    |> Expect.equal [ "d4", "d6", "d8", "d10" ]
            )
        ]
