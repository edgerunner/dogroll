module Conflict.View exposing (Config, view)

import Conflict exposing (Raise, State)
import Dice exposing (Dice)
import Die exposing (Die)
import Die.View
import Html exposing (Html)
import Html.Attributes as Attr
import UI


type alias Config msg =
    { takeMoreDice : msg
    , playDie : Die -> msg
    , noop : msg
    }


view : Config msg -> State -> Html msg
view config state =
    [ takeMoreDiceButton config.takeMoreDice
    , diceSet config.playDie state.proponent.pool
    , playArea state.raise
    , diceSet (always config.noop) state.opponent.pool
    ]
        |> Html.main_ [ Attr.id "conflict" ]


takeMoreDiceButton : msg -> Html msg
takeMoreDiceButton onClick =
    UI.button "Take More Dice"
        |> Html.map (always onClick)


diceSet : (Die -> msg) -> Dice -> Html msg
diceSet onClick =
    Dice.toList
        >> List.map (Die.View.for Die.View.regular)
        >> Html.section [ Attr.id "my-dice" ]
        >> Html.map onClick


playArea : Raise -> Html msg
playArea _ =
    UI.pool [ UI.poolCaption "Choose dice for your raise" ]
