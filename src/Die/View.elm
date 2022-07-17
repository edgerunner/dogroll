module Die.View exposing (Click, for, generic)

import Die exposing (Die)
import Die.Size exposing (Size)
import Html exposing (Attribute, Html)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events as Event


type alias Click =
    ()


generic : Size -> String -> Html Click
generic size =
    Svg.text
        >> List.singleton
        >> Svg.text_ []
        >> List.singleton
        >> Svg.svg
            [ Attr.viewBox "0 0 64 64"
            , Attr.class "die"
            , Attr.class (Die.Size.toString size)
            , Event.onClick ()
            ]


for : Die -> Html Click
for die =
    generic
        (die |> Die.size)
        (die |> Die.face |> String.fromInt)
