module Dice.Pips exposing (Pips, add, decrement, five, four, fromInt, increment, one, three, toInt, toList, two, zero)


type Pips
    = Pips (List ())


zero : Pips
zero =
    Pips []


one : Pips
one =
    Pips [ () ]


two : Pips
two =
    Pips [ (), () ]


three : Pips
three =
    Pips [ (), (), () ]


four : Pips
four =
    Pips [ (), (), (), () ]


five : Pips
five =
    Pips [ (), (), (), (), () ]


increment : Pips -> Pips
increment (Pips pips) =
    () :: pips |> Pips


decrement : Pips -> Pips
decrement (Pips pips) =
    List.drop 1 pips |> Pips


add : Pips -> Pips -> Pips
add (Pips p1) (Pips p2) =
    (p1 ++ p2) |> Pips


toInt : Pips -> Int
toInt (Pips pips) =
    List.length pips


toList : Pips -> List ()
toList (Pips pips) =
    pips


fromInt : Int -> Pips
fromInt =
    List.repeat >> (|>) () >> Pips
