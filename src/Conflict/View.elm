module Conflict.View exposing (Config, view)

import Conflict exposing (Raise(..), See(..), Side(..), State)
import Dice exposing (Dice)
import Die exposing (Die, Rolled)
import Die.Size exposing (Size)
import Die.View
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Event
import Pips
import UI


type alias Config msg =
    { takeMoreDice : msg
    , playDie : Die Rolled -> msg
    , raise : msg
    , see : msg
    , fallout : Size -> msg
    , give : msg
    , restart : msg
    , noop : msg
    , mySide : Maybe Side
    }


view : Config msg -> State -> Html msg
view config state =
    [ Html.button [ Attr.class "give", Event.onClick config.give ] [ Html.text "Give" ]
    , takeMoreDiceButton
        |> Html.map (always config.takeMoreDice)
    , config.mySide
        |> Maybe.withDefault Conflict.proponent
        |> Conflict.player
        |> (|>) state
        |> .pool
        |> diceSet "my-dice"
        |> Html.map config.playDie
    , playArea config state
        |> Html.map (always config.noop)
    , if config.mySide == Just state.go then
        actionButton config state.raise

      else
        Html.text ""
    , config.mySide
        |> Maybe.withDefault Conflict.proponent
        |> Conflict.otherSide
        |> Conflict.player
        |> (|>) state
        |> .pool
        |> diceSet "their-dice"
        |> Html.map (always config.noop)
    ]
        |> Html.main_ [ Attr.id "conflict", sideClass config.mySide ]


sideClass : Maybe Side -> Attribute msg
sideClass side =
    Attr.class <|
        case side of
            Nothing ->
                "spectator"

            Just Proponent ->
                "proponent"

            Just Opponent ->
                "opponent"


actionButton : Config msg -> Raise -> Html msg
actionButton config raise =
    case raise of
        ReadyToRaise _ _ ->
            UI.button "Raise"
                |> Html.map (always config.raise)

        RaisedWith _ _ LoseTheStakes ->
            UI.button "Lose the Stakes"
                |> Html.map (always config.give)

        RaisedWith _ _ see ->
            Html.map (always config.see) <|
                case see of
                    LoseTheStakes ->
                        Html.text ""

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


diceSet : String -> Dice Rolled -> Html (Die Rolled)
diceSet id =
    Dice.toList
        >> List.map (Die.View.rolled Die.View.regular)
        >> UI.pile id identity


type alias TextFor =
    { myTurn : String
    , notMyTurn : ( String, String )
    }


textGetter : Maybe Side -> Side -> TextFor -> String
textGetter mySide go =
    case Maybe.map ((==) go) mySide of
        Just True ->
            .myTurn

        Just False ->
            .notMyTurn >> (\( pre, post ) -> [ pre, "other side", post ] |> String.concat)

        Nothing ->
            .notMyTurn
                >> (\( pre, post ) ->
                        [ pre
                        , case go of
                            Proponent ->
                                "proponent"

                            Opponent ->
                                "opponent"
                        , post
                        ]
                            |> String.concat
                   )


playArea : Config msg -> State -> Html (Die Rolled)
playArea config state =
    let
        caption =
            textGetter config.mySide state.go
                >> UI.poolCaption

        raiseRecommendations =
            Conflict.player state.go state
                |> .pool
                |> Dice.best 2
                |> Dice.toList
                |> List.map (Die.View.rolled Die.View.faded)
    in
    UI.pool <|
        case state.raise of
            PendingTwoDice ->
                ({ myTurn = "Play two dice to raise"
                 , notMyTurn = ( "Waiting for the ", " to raise" )
                 }
                    |> caption
                )
                    :: raiseRecommendations

            PendingOneDie die1 ->
                [ { myTurn = "Play one die to raise"
                  , notMyTurn = ( "Waiting for the ", " to raise" )
                  }
                    |> caption
                , Die.View.rolled Die.View.regular die1
                ]
                    ++ List.take 1 raiseRecommendations

            ReadyToRaise die1 die2 ->
                [ { myTurn = "Go ahead and raise"
                  , notMyTurn = ( "Waiting for the ", " to raise" )
                  }
                    |> caption
                , Die.View.rolled Die.View.regular die1
                , Die.View.rolled Die.View.regular die2
                ]

            RaisedWith raise1 raise2 see ->
                let
                    seeCaption =
                        caption
                            { myTurn = "Play dice to see the raise"
                            , notMyTurn = ( "Waiting for the ", " to see" )
                            }
                in
                case see of
                    LoseTheStakes ->
                        [ Die.View.rolled Die.View.regular raise1
                        , Die.View.rolled Die.View.regular raise2
                        , seeCaption
                        ]

                    ReverseTheBlow see1 ->
                        [ Die.View.rolled Die.View.regular raise1
                        , Die.View.rolled Die.View.regular raise2
                        , seeCaption
                        , Die.View.rolled Die.View.regular see1
                        ]

                    BlockOrDodge see1 see2 ->
                        [ Die.View.rolled Die.View.regular raise1
                        , Die.View.rolled Die.View.regular raise2
                        , seeCaption
                        , Die.View.rolled Die.View.regular see1
                        , Die.View.rolled Die.View.regular see2
                        ]

                    TakeTheBlow see1 see2 see3 seeMore ->
                        [ Die.View.rolled Die.View.regular raise1
                        , Die.View.rolled Die.View.regular raise2
                        , seeCaption
                        , Die.View.rolled Die.View.regular see1
                        , Die.View.rolled Die.View.regular see2
                        , Die.View.rolled Die.View.regular see3
                        ]
                            ++ List.map (Die.View.rolled Die.View.regular) seeMore

            PendingFallout pips ->
                [ { myTurn = "Take fallout dice to continue"
                  , notMyTurn = ( "Waiting for the ", " to take fallout dice" )
                  }
                    |> caption
                , UI.poolCaption (Pips.repeat "✖︎" pips |> String.join " ")
                ]

            GivenUp Nothing ->
                [ UI.poolCaption "This conflict is over" ]

            GivenUp (Just die) ->
                [ { myTurn = "You can take your best die to a follow-up conflict"
                  , notMyTurn = ( "The ", " can take their best die to a follow-up conflict" )
                  }
                    |> caption
                , Die.View.rolled Die.View.regular die
                ]
