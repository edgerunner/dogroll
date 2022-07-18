module Setup exposing (Config, Model, empty, increment, roll, view)

import Dice exposing (Dice)
import Dice.Pips exposing (Pips)
import Die.Size exposing (Size(..))
import Die.View
import Html exposing (Html)
import Html.Attributes as Attr
import Random exposing (Seed)
import UI


type alias Model =
    { d4 : Pips
    , d6 : Pips
    , d8 : Pips
    , d10 : Pips
    }


type alias Config msg =
    { increment : Size -> msg
    , roll : msg
    }


empty : Model
empty =
    { d4 = Dice.Pips.zero
    , d6 = Dice.Pips.zero
    , d8 = Dice.Pips.zero
    , d10 = Dice.Pips.zero
    }


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
            |> Html.map config.increment
        ]


takenDiceView : Model -> List (Html Size)
takenDiceView model =
    Die.Size.all
        |> List.map (\size -> takenDiceStack size (Die.Size.get size model))


takenDiceStack : Size -> Pips -> Html Size
takenDiceStack size =
    (Die.View.generic Die.View.regular size " "
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
increment size model =
    case size of
        D4 ->
            { model | d4 = Dice.Pips.grow model.d4 }

        D6 ->
            { model | d6 = Dice.Pips.grow model.d6 }

        D8 ->
            { model | d8 = Dice.Pips.grow model.d8 }

        D10 ->
            { model | d10 = Dice.Pips.grow model.d10 }


roll : Model -> Seed -> Dice
roll model seed =
    Die.Size.all
        |> List.map
            (\size ->
                size
                    |> Die.Size.get
                    |> (|>) model
                    |> Dice.Pips.toInt
                    |> Dice.init seed
                    |> (|>) size
            )
        |> Dice.combine
        |> Dice.roll
