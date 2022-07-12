module Tests.Die exposing (suite)

import Die
import Die.Size exposing (Size(..))
import Expect
import Test exposing (Test, describe, fuzz, test)
import Tests.Helpers as Helpers


suite : Test
suite =
    describe "Die"
        [ test "reporting die size"
            (\() ->
                Die.Size.all
                    |> List.map (\size -> Die.init size 0 |> Die.size)
                    |> Expect.equal [ D10, D8, D6, D4 ]
            )
        , fuzz Helpers.dieFuzzer
            "rolling"
            (Die.roll
                >> (\die ->
                        Die.face die
                            |> Helpers.between1and (Die.size die |> Die.Size.toInt)
                   )
            )
        , fuzz Helpers.dieFuzzer
            "starting with a rolled face"
            (\die ->
                Die.face die
                    |> Helpers.between1and (Die.size die |> Die.Size.toInt)
            )
        , test "string representation"
            (\() ->
                Die.Size.all
                    |> List.map (\size -> Die.init size 0 |> Die.toString)
                    |> Expect.equal [ "d10", "d8", "d6", "d4" ]
            )
        ]
