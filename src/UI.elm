module UI exposing (button)

import Html exposing (Html)
import Html.Events


type alias Click =
    ()


button : String -> Html Click
button =
    Html.text
        >> List.singleton
        >> Html.button
            [ Html.Events.onClick () ]
