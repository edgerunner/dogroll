module Setup exposing (Config, Model, decrement, empty, increment, toDice, view)

import Dice exposing (Dice)
import Die
import Die.Size exposing (Size)
import Die.View
import Html exposing (Html)
import Html.Attributes as Attr
import UI


type alias Model =
    Dice


type alias Config msg =
    { increment : Size -> msg
    , decrement : Size -> msg
    , roll : msg
    }


empty : Model
empty =
    Dice.empty


view : Config msg -> Model -> Html msg
view config model =
    Html.main_ [ Attr.id "setup" ]
        [ UI.pool
            [ UI.poolCaption "Take some dice"
            , freshDiceView
                |> Html.map config.increment
            ]
        , Html.section
            [ Attr.class "dice" ]
            (takenDiceView model)
            |> Html.map config.decrement
        , UI.button "Roll the dice"
            |> Html.map (always config.roll)
        ]


takenDiceView : Model -> List (Html Size)
takenDiceView model =
    Die.Size.all
        |> List.map (\size -> takenDiceStack size model)


takenDiceStack : Size -> Model -> Html Size
takenDiceStack size =
    Dice.toList
        >> List.filter (Die.size >> (==) size)
        >> (Die.View.generic Die.View.regular size " "
                |> always
                |> List.map
           )
        >> Html.div []
        >> Html.map (always size)


freshDiceView : Html Size
freshDiceView =
    Die.Size.all
        |> List.map
            (\size ->
                Die.View.generic Die.View.regular size "+"
                    |> Html.map (always size)
            )
        |> Html.div []


increment : Size -> Model -> Model
increment size =
    Die.init size |> Dice.add


decrement : Size -> Model -> Model
decrement size =
    Die.init size |> Dice.drop


toDice : Model -> Dice
toDice =
    identity
