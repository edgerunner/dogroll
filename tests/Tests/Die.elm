module Tests.Die exposing (suite)

import Die exposing (Size(..))
import Expect
import Test exposing (Test, describe, fuzz, test)
import Tests.Helpers as Helpers


suite : Test
suite =
    describe "Die"
        [ test "reporting die size"
            (\() ->
                Helpers.dieInitializers
                    |> List.map ((|>) 0)
                    |> List.map Die.size
                    |> Expect.equal [ D4, D6, D8, D10 ]
            )
        , fuzz Helpers.dieFuzzer
            "rolling"
            (Die.roll
                >> (\die ->
                        Die.face die
                            |> Helpers.between1and (Die.faces die)
                   )
            )
        , fuzz Helpers.dieFuzzer
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
                Helpers.dieInitializers
                    |> List.map ((|>) 0 >> Die.toString)
                    |> Expect.equal [ "d4", "d6", "d8", "d10" ]
            )
        ]
