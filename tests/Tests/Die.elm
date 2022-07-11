module Tests.Die exposing (suite)

import Die exposing (Die, Size(..))
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)
import Tests.Helpers as Helpers


dice : List (Int -> Die)
dice =
    [ Die.d4, Die.d6, Die.d8, Die.d10 ]


multipleDice : Fuzzer Die
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
                            |> Helpers.between1and (Die.faces die)
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
