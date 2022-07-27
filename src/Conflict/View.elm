module Conflict.View exposing (Config, view)

import Conflict exposing (Raise(..), See(..), State)
import Dice exposing (Dice)
import Die exposing (Die)
import Die.Size exposing (Size)
import Die.View
import Html exposing (Html)
import Html.Attributes as Attr
import Pips
import UI


type alias Config msg =
    { takeMoreDice : msg
    , playDie : Die -> msg
    , raise : msg
    , see : msg
    , fallout : Size -> msg
    , restart : msg
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

        RaisedWith _ _ see ->
            Html.map (always config.see) <|
                case see of
                    LoseTheStakes ->
                        UI.button "Lose the Stakes"

                    ReverseTheBlow _ ->
                        UI.button "Reverse the Blow"

                    BlockOrDodge _ _ ->
                        UI.button "Block or Dodge"

                    TakeTheBlow _ _ _ _ ->
                        UI.button "Take the Blow"

        PendingFallout pips ->
            Die.Size.all
                |> List.map
                    (\size ->
                        Die.View.generic
                            Die.View.faded
                            size
                            (Pips.toInt pips |> String.fromInt)
                            |> Html.map (always <| config.fallout size)
                    )
                |> Html.div [ Attr.id "fallout-selector" ]

        GivenUp _ ->
            UI.button "Start another conflict"
                |> Html.map (always config.restart)

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

                    ReverseTheBlow see1 ->
                        [ Die.View.for Die.View.regular raise1
                        , Die.View.for Die.View.regular raise2
                        , UI.poolCaption "Play dice to see the raise"
                        , Die.View.for Die.View.regular see1
                        ]

                    BlockOrDodge see1 see2 ->
                        [ Die.View.for Die.View.regular raise1
                        , Die.View.for Die.View.regular raise2
                        , UI.poolCaption "Play dice to see the raise"
                        , Die.View.for Die.View.regular see1
                        , Die.View.for Die.View.regular see2
                        ]

                    TakeTheBlow see1 see2 see3 seeMore ->
                        [ Die.View.for Die.View.regular raise1
                        , Die.View.for Die.View.regular raise2
                        , UI.poolCaption "Play dice to see the raise"
                        , Die.View.for Die.View.regular see1
                        , Die.View.for Die.View.regular see2
                        , Die.View.for Die.View.regular see3
                        ]
                            ++ List.map (Die.View.for Die.View.regular) seeMore

            PendingFallout pips ->
                [ UI.poolCaption "Take fallout dice to continue"
                , UI.poolCaption (Pips.repeat "✖︎" pips |> String.join " ")
                ]

            GivenUp Nothing ->
                [ UI.poolCaption "This conflict is over" ]

            GivenUp (Just die) ->
                [ UI.poolCaption "This conflict is over"
                , Die.View.for Die.View.regular die
                ]
