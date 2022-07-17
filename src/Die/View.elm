module Die.View exposing (Click, faded, for, generic, regular)

import Die exposing (Die)
import Die.Size exposing (Size)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Event


type alias Click =
    ()


generic : Size -> String -> Style -> Html Click
generic size face style_ =
    Html.text face
        |> List.singleton
        |> Html.node "data"
            [ Attr.class "die"
            , Attr.class (Die.Size.toString size)
            , style style_
            , Event.onClick ()
            ]


for : Die -> Style -> Html Click
for die =
    generic
        (die |> Die.size)
        (die |> Die.face |> String.fromInt)


type Style
    = Style String


regular : Style
regular =
    Style ""


faded : Style
faded =
    Style "faded"


style : Style -> Attribute msg
style (Style s) =
    Attr.class s
