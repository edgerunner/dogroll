module Conflict.View exposing (Config, view)

import Conflict exposing (Raise(..), See(..), State)
import Dice exposing (Dice)
import Die exposing (Die)
import Die.View
import Html exposing (Html)
import Html.Attributes as Attr
import UI


type alias Config msg =
    { takeMoreDice : msg
    , playDie : Die -> msg
    , raise : msg
    , noop : msg
    }


view : Config msg -> State -> Html msg
view config state =
    [ takeMoreDiceButton
        |> Html.map (always config.takeMoreDice)
    , diceSet "my-dice" state.proponent.pool
        |> Html.map config.playDie
    , playArea state.raise
        |> Html.map (always config.noop)
    , actionButton config state.raise
    , diceSet "their-dice" state.opponent.pool
        |> Html.map (always config.noop)
    ]
        |> Html.main_ [ Attr.id "conflict" ]


actionButton : Config msg -> Raise -> Html msg
actionButton config raise =
    case raise of
        ReadyToRaise _ _ ->
            UI.button "Raise"
                |> Html.map (always config.raise)

        _ ->
            Html.text ""


takeMoreDiceButton : Html ()
takeMoreDiceButton =
    UI.button "Take More Dice"


diceSet : String -> Dice -> Html Die
diceSet id =
    Dice.toList
        >> List.map (Die.View.for Die.View.regular)
        >> Html.section [ Attr.id id ]


playArea : Raise -> Html Die
playArea raise =
    UI.pool <|
        case raise of
            PendingTwoDice ->
                [ UI.poolCaption "Play two dice to raise" ]

            PendingOneDie die1 ->
                [ UI.poolCaption "Play one die to raise"
                , Die.View.for Die.View.regular die1
                ]

            ReadyToRaise die1 die2 ->
                [ UI.poolCaption "Ready to raise"
                , Die.View.for Die.View.regular die1
                , Die.View.for Die.View.regular die2
                ]

            RaisedWith raise1 raise2 see ->
                case see of
                    LoseTheStakes ->
                        [ Die.View.for Die.View.regular raise1
                        , Die.View.for Die.View.regular raise2
                        , UI.poolCaption "Play dice to see the raise"
                        ]

                    ReverseTheBlow _ ->
                        Debug.todo "branch 'ReverseTheBlow _' not implemented"

                    BlockOrDodge _ _ ->
                        Debug.todo "branch 'BlockOrDodge _ _' not implemented"

                    TakeTheBlow _ _ _ _ ->
                        Debug.todo "branch 'TakeTheBlow _ _ _ _' not implemented"

            PendingFallout _ ->
                Debug.todo "branch 'PendingFallout _' not implemented"

            GivenUp _ ->
                Debug.todo "branch 'GivenUp _' not implemented"
