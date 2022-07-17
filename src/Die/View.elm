module Die.View exposing (Click, default)

import Die exposing (Die)
import Html exposing (Attribute, Html)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events as Event


type alias Click =
    ()


default : Html Click
default =
    Svg.svg
        [ Attr.viewBox "0 0 64 64"
        , Attr.class "die d4"
        , Event.onClick ()
        ]
        [ Svg.text_ [] [ Svg.text "4" ] ]
