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


increment : Pips -> Pips
increment =
    (::) ()


decrement : Pips -> Pips
decrement =
    List.drop 1


add : Pips -> Pips -> Pips
add =
    (++)


toInt : Pips -> Int
toInt =
    List.length
