module Tests.Die exposing (..)

import Die exposing (Die, Size(..))
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
        [ test "reporting die size"
            (\() ->
                dice
                    |> List.map ((|>) 0)
                    |> List.map Die.size
                    |> Expect.equal [ D4, D6, D8, D10 ]
            )
        , fuzz multipleDice
            "rolling"
            (Die.roll
                >> (\die ->
                        Die.face die
                            |> Expect.all
                                [ Expect.atLeast 1
                                , Expect.atMost (Die.faces die)
                                ]
                   )
            )
        , fuzz multipleDice
            "starting with a rolled face"
            (\die ->
                Die.face die
                    |> Expect.all
                        [ Expect.atLeast 1
                        , Expect.atMost (Die.faces die)
                        ]
            )
        , test "string representation"
            (\() ->
                dice
                    |> List.map ((|>) 0 >> Die.toString)
                    |> Expect.equal [ "d4", "d6", "d8", "d10" ]
            )
        ]
