module Conflict.View exposing (Config, view)

import Conflict exposing (Raise(..), See(..), Side(..))
import Conflict.Manager as Manager
import Dice exposing (Dice)
import Die exposing (Die, Rolled)
import Die.Size exposing (Size)
import Die.View
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
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
    , participate : Side -> msg
    , noop : msg
    }


view : Config msg -> Manager.State -> Html msg
view config state =
    case state of
        Manager.InProgress progressState ->
            progressView config progressState

        Manager.PendingParticipants pendingState ->
            pendingView config pendingState

        Manager.Finished finishedState ->
            finishedView config finishedState


finishedView : Config msg -> Manager.FinishedState -> Html msg
finishedView config state =
    case state.followUp of
        Nothing ->
            Html.main_ [ Attr.id "finished" ]
                [ UI.pool [ UI.poolCaption "This conflict is over" ]
                , UI.button "Start a new conflict"
                    |> Html.map (always config.restart)
                ]

        Just ( side, die ) ->
            case state.you |> Maybe.map ((==) side) of
                Just True ->
                    Html.main_ [ Attr.id "finished", Attr.class "follow-up" ]
                        [ UI.pool
                            [ UI.poolCaption "You can keep this die"
                            , Die.View.rolled Die.View.regular die
                                |> Html.map (always config.restart)
                            ]
                        , UI.button "Start follow-up conflict"
                            |> Html.map (always config.restart)
                        ]

                _ ->
                    Html.main_ [ Attr.id "finished" ]
                        [ UI.pool [ UI.poolCaption "This conflict is over" ]
                        , UI.button "Start a new conflict"
                            |> Html.map (always config.restart)
                        ]


progressView : Config msg -> Manager.InProgressState -> Html msg
progressView config state =
    [ UI.button "Give"
        |> Html.map (always config.give)
    , UI.button "Take some dice"
        |> Html.map (always config.takeMoreDice)
    , state.you
        |> Maybe.withDefault Conflict.proponent
        |> Conflict.player
        |> with state.conflict
        |> .pool
        |> diceSet "my-dice"
        |> Html.map config.playDie
    , playArea config state
    , if state.you == Just state.conflict.go then
        actionButton config state.conflict.raise

      else
        Html.text ""
    , state.you
        |> Maybe.withDefault Conflict.proponent
        |> Conflict.otherSide
        |> Conflict.player
        |> with state.conflict
        |> .pool
        |> diceSet "their-dice"
        |> Html.map (always config.noop)
    ]
        |> Html.main_
            [ Attr.id "conflict"
            , sideClass state.you
            , turnClass state.you state.conflict.go
            ]


turnClass : Maybe Side -> Side -> Attribute msg
turnClass mySide go =
    case
        mySide
            |> Maybe.map ((==) go)
    of
        Just True ->
            Attr.class "my-turn"

        Just False ->
            Attr.class "their-turn"

        Nothing ->
            Attr.class "no-turn"


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

        _ ->
            Html.text ""


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


playArea : Config msg -> Manager.InProgressState -> Html msg
playArea config state =
    let
        caption =
            textGetter state.you state.conflict.go
                >> UI.poolCaption

        raiseRecommendations =
            Conflict.player state.conflict.go state.conflict
                |> .pool
                |> Dice.best 2
                |> Dice.toList
                |> List.map
                    (Die.View.rolled Die.View.faded
                        >> Html.map config.playDie
                    )

        noClick =
            Html.map (always config.noop)
    in
    UI.pool <|
        case state.conflict.raise of
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
                , Die.View.rolled Die.View.regular die1 |> noClick
                ]
                    ++ List.take 1 raiseRecommendations

            ReadyToRaise die1 die2 ->
                [ { myTurn = "Go ahead and raise"
                  , notMyTurn = ( "Waiting for the ", " to raise" )
                  }
                    |> caption
                , Die.View.rolled Die.View.regular die1 |> noClick
                , Die.View.rolled Die.View.regular die2 |> noClick
                ]

            RaisedWith raise1 raise2 see ->
                let
                    seeCaption =
                        caption
                            { myTurn = "Play dice to see the raise"
                            , notMyTurn = ( "Waiting for the ", " to see" )
                            }

                    raiseDice =
                        [ Die.View.rolled Die.View.regular raise1 |> noClick
                        , Die.View.rolled Die.View.regular raise2 |> noClick
                        ]

                    raiseValue =
                        Die.face raise1 + Die.face raise2

                    seeRecommendations seen =
                        let
                            seeValue =
                                List.foldl
                                    (\seeDie remaining ->
                                        remaining - Die.face seeDie
                                    )
                                    raiseValue
                                    seen
                        in
                        Conflict.player state.conflict.go state.conflict
                            |> .pool
                            |> Dice.match seeValue
                            |> Dice.toList
                            |> List.map
                                (Die.View.rolled Die.View.faded
                                    >> Html.map config.playDie
                                )
                in
                case see of
                    LoseTheStakes ->
                        raiseDice
                            ++ seeCaption
                            :: seeRecommendations []

                    ReverseTheBlow see1 ->
                        raiseDice
                            ++ seeCaption
                            :: (Die.View.rolled Die.View.regular see1 |> noClick)
                            :: seeRecommendations [ see1 ]

                    BlockOrDodge see1 see2 ->
                        raiseDice
                            ++ seeCaption
                            :: (Die.View.rolled Die.View.regular see1 |> noClick)
                            :: (Die.View.rolled Die.View.regular see2 |> noClick)
                            :: seeRecommendations [ see1, see2 ]

                    TakeTheBlow see1 see2 see3 seeMore ->
                        raiseDice
                            ++ seeCaption
                            :: (Die.View.rolled Die.View.regular see1 |> noClick)
                            :: (Die.View.rolled Die.View.regular see2 |> noClick)
                            :: (Die.View.rolled Die.View.regular see3 |> noClick)
                            :: List.map (Die.View.rolled Die.View.regular >> noClick) seeMore
                            ++ seeRecommendations (see1 :: see2 :: see3 :: seeMore)

            PendingFallout pips ->
                [ { myTurn = "Take fallout dice to continue"
                  , notMyTurn = ( "Waiting for the ", " to take fallout dice" )
                  }
                    |> caption
                , UI.poolCaption (Pips.repeat "✖︎" pips |> String.join " ")
                ]

            GivenUp _ ->
                [ UI.poolCaption "This conflict is over" ]


pendingView : Config msg -> Manager.PendingParticipantsState -> Html msg
pendingView config state =
    [ joinButtons config state ]
        |> Html.main_ [ Attr.id "pending" ]


joinButtons : Config msg -> Manager.PendingParticipantsState -> Html msg
joinButtons config state =
    Html.div [ Attr.id "join-buttons" ]
        (case state.side of
            Nothing ->
                [ Html.h4 [] [ Html.text "Participate as…" ]
                , UI.button "Proponent"
                    |> Html.map (always <| config.participate Conflict.proponent)
                , UI.button "Opponent"
                    |> Html.map (always <| config.participate Conflict.opponent)
                ]

            Just (Err Proponent) ->
                [ Html.h4 [] [ Html.text "Participate as…" ]
                , UI.button "Opponent"
                    |> Html.map (always <| config.participate Conflict.opponent)
                ]

            Just (Err Opponent) ->
                [ Html.h4 [] [ Html.text "Participate as…" ]
                , UI.button "Proponent"
                    |> Html.map (always <| config.participate Conflict.proponent)
                ]

            Just (Ok Proponent) ->
                [ Html.h4 [] [ Html.text "Waiting for the opponent…" ]
                ]

            Just (Ok Opponent) ->
                [ Html.h4 [] [ Html.text "Waiting for the proponent…" ]
                ]
        )


with : a -> (a -> b) -> b
with =
    (|>)
