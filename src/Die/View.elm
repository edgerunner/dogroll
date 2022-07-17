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
        ]
        [ Svg.text_ [] [ Svg.text "4" ] ]


path : { d4 : Attribute msg }
path =
    { d4 =
        [ "M50.0395 45.5184"
        , "L33.2545 16.4464"
        , "C32.1545 14.5414 30.3545 14.5414 29.2545 16.4464"
        , "L12.4695 45.5184"
        , "C11.3695 47.4234 12.2695 48.9824 14.4695 48.9824"
        , "H48.0385"
        , "C50.2395 48.9824 51.1395 47.4234 50.0395 45.5184"
        , "Z"
        ]
            |> String.concat
            |> Attr.d
    }
