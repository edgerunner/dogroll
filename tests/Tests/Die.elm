module Tests.Die exposing (..)

import Die

import Expect
import Test exposing (Test, describe, test, todo)


suite : Test
suite =
    describe "Die"
        [ test "reporting number of sides"
            (\() ->
                [Die.d4, Die.d6, Die.d8, Die.d10]
                    |> List.map Die.sides
                    |> Expect.equal [4, 6, 8, 10]
            )
        , todo "roll"
        , todo "report last roll"
        ]