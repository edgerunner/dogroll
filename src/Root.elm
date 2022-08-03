module Root exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Url.Builder


view : Maybe String -> Html msg
view newConflictId =
    [ Html.h1 [ Attr.id "title" ] [ Html.text "Dogroll" ]
    , Html.p []
        [ Html.text "A remote-capable dice roller for "
        , Html.em [] [ Html.text "Dogs in the Vineyard" ]
        , Html.text " by Vincent Baker"
        ]
    , newConflictId
        |> Maybe.map newConflictLink
        |> Maybe.withDefault (Html.text "")
    ]
        |> Html.main_ [ Attr.id "root" ]


newConflictLink : String -> Html msg
newConflictLink newConflictId =
    Html.a
        [ Url.Builder.absolute [ newConflictId ] []
            |> Attr.href
        ]
        [ Html.text "New Conflict" ]
