module Tests.Dice exposing (suite)

import Dice exposing (Dice)
import Dice.Pips as Pips
import Dice.Type exposing (DemonicInfluence(..), Gun(..), Quality(..), Stat(..), Type(..))
import Die
import Die.Size
import Expect exposing (Expectation)
import Random exposing (Seed)
import Test exposing (Test, describe, test)
import Tests.Helpers as Helpers


fuzzRolls : String -> (Seed -> Dice -> Expectation) -> Test
fuzzRolls =
    Test.fuzz2 Helpers.seedFuzzer Helpers.diceFuzzer


suite : Test
suite =
    describe "Dice"
        [ test "initializing multiple dice"
            (\_ ->
                Dice.init Die.Size.D6 Pips.four
                    |> Dice.toList
                    |> Expect.equalLists (List.repeat 4 (Die.init Die.Size.D6))
            )
        , test "combining dice"
            (\_ ->
                Dice.combine
                    [ Dice.init Die.Size.D8 Pips.two
                    , Dice.init Die.Size.D4 Pips.one
                    ]
                    |> Dice.toList
                    |> Expect.equalLists
                        [ Die.init Die.Size.D8
                        , Die.init Die.Size.D8
                        , Die.init Die.Size.D4
                        ]
            )
        , fuzzRolls
            "rolling all dice at once"
            (\seed ->
                Dice.roll seed
                    >> Dice.toList
                    >> List.map
                        (\die ->
                            Helpers.between1and
                                (Die.size die |> Die.Size.toInt)
                                (Die.face die |> Maybe.withDefault 0)
                        )
                    >> Helpers.allPass
            )
        , fuzzRolls
            "are always sorted"
            (\seed pool ->
                let
                    larger =
                        pool |> Dice.roll seed |> Dice.toList

                    smaller =
                        larger |> List.drop 1

                    atMost lg sm =
                        Expect.atMost
                            (lg |> Die.face |> Maybe.withDefault 0)
                            (sm |> Die.face |> Maybe.withDefault 11)

                    expectations =
                        List.map2 atMost larger smaller
                in
                Helpers.allPass expectations
            )
        , describe
            "string representation"
            [ test "single type"
                (\_ ->
                    Dice.init Die.Size.D4 Pips.two
                        |> Dice.toString
                        |> Expect.equal "2d4"
                )
            , test "one of single type"
                (\_ ->
                    Dice.init Die.Size.D10 Pips.one
                        |> Dice.toString
                        |> Expect.equal "1d10"
                )
            , test "multiple types"
                (\_ ->
                    Dice.combine
                        [ Dice.init Die.Size.D4 Pips.one
                        , Dice.init Die.Size.D8 Pips.two
                        ]
                        |> Dice.toString
                        |> Expect.equal "2d8+1d4"
                )
            ]
        , describe "Type"
            [ test "Stat dice"
                (\_ ->
                    Stat Acuity Pips.two
                        |> expectDiceFromType "4d6"
                )
            , test "Trait dice"
                (\_ ->
                    Trait "I'm a good shot" Die.Size.D8 Pips.two
                        |> expectDiceFromType "3d8"
                )
            , test "Relationship dice"
                (\_ ->
                    Relationship "My riding instructor" Die.Size.D4 Pips.zero
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
                        test (Dice.Type.toString (DemonicInfluence subject))
                            (always (expectDiceFromType expected (DemonicInfluence subject)))
                    )
                |> describe "Demonic influence"
            ]
        ]


expectDiceFromType : String -> Type -> Expectation
expectDiceFromType name =
    Dice.Type.toDice
        >> Dice.toString
        >> Expect.equal name
