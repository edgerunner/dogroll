module Dice.Pips exposing (..)


type alias Pips =
    List ()


zero : Pips
zero =
    []


one : Pips
one =
    [ () ]


two : Pips
two =
    [ (), () ]


three : Pips
three =
    [ (), (), () ]


four : Pips
four =
    [ (), (), (), () ]


five : Pips
five =
    [ (), (), (), (), () ]


grow : Pips -> Pips
grow =
    (::) ()


add : Pips -> Pips -> Pips
add =
    (++)


toInt : Pips -> Int
toInt =
    List.length
