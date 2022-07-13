module Tests.Dice exposing (suite)

import Dice
import Dice.Pips
import Dice.Type exposing (DemonicInfluence(..), Gun(..), Quality(..), Stat(..), Type(..))
import Die
import Die.Size
import Expect exposing (Expectation)
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
                Dice.init seed 4 Die.Size.D6
                    |> Dice.faces
                    |> List.map (Helpers.between1and 6)
                    |> Helpers.allPass
            )
        , test "combining dice"
            (\_ ->
                Dice.combine
                    [ Dice.init seed 2 Die.Size.D8
                    , Dice.init seed 1 Die.Size.D4
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
                    Dice.init seed 2 Die.Size.D4
                        |> Dice.toString
                        |> Expect.equal "2d4"
                )
            , test "one of single type"
                (\_ ->
                    Dice.init seed 1 Die.Size.D10
                        |> Dice.toString
                        |> Expect.equal "1d10"
                )
            , test "multiple types"
                (\_ ->
                    Dice.combine
                        [ Dice.init seed 1 Die.Size.D4
                        , Dice.init seed 2 Die.Size.D8
                        ]
                        |> Dice.toString
                        |> Expect.equal "2d8+1d4"
                )
            ]
        , describe "Type"
            [ test "Stat dice"
                (\_ ->
                    Stat Acuity Dice.Pips.two
                        |> expectDiceFromType "4d6"
                )
            , test "Trait dice"
                (\_ ->
                    Trait "I'm a good shot" Die.Size.D8 Dice.Pips.two
                        |> expectDiceFromType "3d8"
                )
            , test "Relationship dice"
                (\_ ->
                    Relationship "My riding instructor" Die.Size.D4 Dice.Pips.zero
                        |> expectDiceFromType "1d4"
                )
            , [ ( "Normal thing", Belonging "A horse" Normal NotGun, "1d6" )
              , ( "Excellent thing", Belonging "A fine horse" Excellent NotGun, "2d6" )
              , ( "Big thing", Belonging "Workhorse" Big NotGun, "1d8" )
              , ( "Excellent+Big", Belonging "Heirloom sabre" ExcellentPlusBig NotGun, "2d8" )
              , ( "Crap thing", Belonging "Nag" Crap NotGun, "1d4" )
              , ( "Normal gun", Belonging "A gun" Normal Gun, "1d6+1d4" )
              , ( "Excellent gun", Belonging "Heirloom revolver" Excellent Gun, "2d6+1d4" )
              , ( "Big gun", Belonging "Magnum" Big Gun, "1d8+1d4" )
              , ( "Excellent+Big gun", Belonging "Ornate hand cannon" ExcellentPlusBig Gun, "2d8+1d4" )
              , ( "Crap gun", Belonging "Misfiring muzzle" Crap Gun, "2d4" )
              ]
                |> List.map
                    (\( desc, subject, expected ) ->
                        test desc
                            (always (expectDiceFromType expected subject))
                    )
                |> describe "Belonging dice"
            , test "Initiation dice"
                (\_ ->
                    Initiation
                        |> expectDiceFromType "4d10+4d6"
                )
            , [ ( Injustice, "1d10" )
              , ( DemonicAttacks, "2d10" )
              , ( Heresy, "3d10" )
              , ( Sorcery, "4d10" )
              , ( HateAndMurder, "5d10" )
              ]
                |> List.map
                    (\( subject, expected ) ->
                        test (Dice.Type.name (DemonicInfluence subject))
                            (always (expectDiceFromType expected (DemonicInfluence subject)))
                    )
                |> describe "Demonic influence"
            ]
        ]


expectDiceFromType : String -> Type -> Expectation
expectDiceFromType name =
    Dice.Type.toDice seed
        >> Dice.toString
        >> Expect.equal name
