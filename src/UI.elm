module UI exposing (button, pile, pool, poolCaption)

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


poolCaption : String -> Html msg
poolCaption =
    Html.text
        >> List.singleton
        >> Html.h3 []


pile : String -> (a -> Html msg) -> List a -> Html msg
pile id toHtml =
    List.map toHtml
        >> Html.section [ Attr.class "pile", Attr.id id ]
