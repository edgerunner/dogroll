module Tests.Die exposing (suite)

import Die
import Die.Size exposing (Size(..))
import Expect
import Random exposing (Seed)
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Tests.Helpers as Helpers


seed : Seed
seed =
    Random.initialSeed 42


suite : Test
suite =
    describe "Die"
        [ test "reporting die size"
            (\() ->
                Die.Size.all
                    |> List.map (\size -> Die.init size |> Die.size)
                    |> Expect.equal [ D10, D8, D6, D4 ]
            )
        , fuzz Helpers.dieFuzzer
            "rolling"
            (Die.roll seed
                >> (\die ->
                        Die.face die
                            |> Maybe.withDefault 0
                            |> Helpers.between1and (Die.size die |> Die.Size.toInt)
                   )
            )
        , fuzz2 Helpers.seedFuzzer
            Helpers.dieFuzzer
            "not rolling twice"
            (\seed_ die ->
                let
                    rolledOnce =
                        Die.roll seed_ die

                    anotherSeed =
                        Random.step Random.independentSeed seed_ |> Tuple.first

                    rolledTwice =
                        Die.roll anotherSeed rolledOnce
                in
                Expect.equal (Die.face rolledOnce) (Die.face rolledTwice)
            )
        , fuzz Helpers.dieFuzzer
            "starting without a rolled face"
            (Die.face >> Expect.equal Nothing)
        , test "string representation"
            (\() ->
                Die.Size.all
                    |> List.map (\size -> Die.init size |> Die.toString)
                    |> Expect.equal [ "d10", "d8", "d6", "d4" ]
            )
        ]
