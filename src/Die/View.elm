module Die.View exposing (Click, any, faded, generic, held, regular, rolled)

import Die exposing (Die, Held, Rolled)
import Die.Size exposing (Size)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Event


type alias Click =
    ()


generic : Style -> Size -> String -> Html Click
generic style_ size face =
    Html.text face
        |> List.singleton
        |> Html.node "data"
            [ Attr.class "die"
            , Attr.class (Die.Size.toString size)
            , style style_
            , Event.onClick ()
            ]


held : Style -> Die Held -> Html (Die Held)
held =
    any


rolled : Style -> Die Rolled -> Html (Die Rolled)
rolled =
    any


any : Style -> Die any -> Html (Die any)
any (Style style_) die =
    Die.fold
        (\size ->
            generic
                ([ "held", style_ ] |> String.join " " |> Style)
                size
                " "
        )
        (\size face ->
            generic
                ([ "rolled", style_ ] |> String.join " " |> Style)
                size
                (face |> String.fromInt)
        )
        die
        |> Html.map (always die)


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
