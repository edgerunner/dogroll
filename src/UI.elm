module UI exposing (button, pool)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events


type alias Click =
    ()


button : String -> Html Click
button =
    Html.text
        >> List.singleton
        >> Html.button
            [ Html.Events.onClick () ]


pool : List (Html msg) -> Html msg
pool =
    Html.section
        [ Attr.class "pool"
        , Attr.class "raised"
        ]
