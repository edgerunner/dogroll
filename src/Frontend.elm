module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Conflict.Manager exposing (State(..))
import Conflict.View
import Die.Size exposing (Size(..))
import Lamdera exposing (Key, sendToBackend)
import Root
import TakeDice
import Types exposing (..)
import UI
import Url exposing (Url)
import Url.Parser


type alias Model =
    FrontendModel


app :
    { init : Lamdera.Url -> Key -> ( Model, Cmd FrontendMsg )
    , view : Model -> Browser.Document FrontendMsg
    , update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
    , subscriptions : Model -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = always Sub.none
        , view = view
        }


init : Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        conflictId =
            Url.Parser.parse Url.Parser.string url
    in
    ( { key = key
      , page =
            conflictId
                |> Maybe.map LoadingConflict
                |> Maybe.withDefault (Root Nothing)
      }
    , conflictId
        |> Maybe.map (ClientInitialized >> Lamdera.sendToBackend)
        |> Maybe.withDefault
            (Lamdera.sendToBackend ClientRequestedRandomConflictId)
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.load (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        ( UserClickedIncrementDie size, Conflict conflict (Just takeDice) ) ->
            { model | page = TakeDice.increment size takeDice |> Just |> Conflict conflict } |> noCmd

        ( UserClickedDecrementDie size, Conflict conflict (Just takeDice) ) ->
            { model | page = TakeDice.decrement size takeDice |> Just |> Conflict conflict } |> noCmd

        ( UserClickedRollDice, Conflict (InProgress conflict) (Just takeDice) ) ->
            sendToBackend (ForConflict conflict.id <| UserWantsToRollDice takeDice)
                |> Tuple.pair { model | page = Conflict (InProgress conflict) Nothing }

        ( UserClickedTakeMoreDice, Conflict conflict Nothing ) ->
            { model | page = Conflict conflict <| Just TakeDice.empty } |> noCmd

        ( UserClickedPlayDie die, Conflict (InProgress conflict) Nothing ) ->
            ( model, sendToBackend (ForConflict conflict.id <| UserWantsToPlayDie die) )

        ( UserClickedRaise, Conflict (InProgress conflict) Nothing ) ->
            ( model, sendToBackend (ForConflict conflict.id <| UserWantsToRaise) )

        ( UserClickedSee, Conflict (InProgress conflict) Nothing ) ->
            ( model, sendToBackend (ForConflict conflict.id <| UserWantsToSee) )

        ( UserClickedFalloutSize size, Conflict (InProgress conflict) Nothing ) ->
            ( model, sendToBackend (ForConflict conflict.id <| UserWantsToSelectFalloutDice size) )

        ( UserClickedGive, Conflict (InProgress conflict) Nothing ) ->
            ( model, sendToBackend (ForConflict conflict.id <| UserWantsToGive) )

        ( UserClickedRestart, Conflict (Finished conflict) Nothing ) ->
            ( model, sendToBackend (ForConflict conflict.id <| UserWantsToFollowUp) )

        ( UserClickedParticipate side, Conflict (PendingParticipants conflict) Nothing ) ->
            ( model, sendToBackend (ForConflict conflict.id <| UserWantsToParticipate side) )

        _ ->
            ( model, Cmd.none )


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model.page ) of
        ( StateUpdated newConflictState, Conflict conflict maybeTakeDice ) ->
            if Conflict.Manager.stateId newConflictState == Conflict.Manager.stateId conflict then
                ( { model | page = Conflict newConflictState maybeTakeDice }, Cmd.none )

            else
                ( model, Cmd.none )

        ( StateUpdated newConflictState, LoadingConflict conflictId ) ->
            if Conflict.Manager.stateId newConflictState == conflictId then
                ( { model | page = Conflict newConflictState Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        ( ErrorReported _, _ ) ->
            -- TODO: show error
            ( model, Cmd.none )

        ( ConflictNotFound _, _ ) ->
            -- TODO: show error
            ( model, Cmd.none )

        ( RandomConflictIdGenerated conflictId, Root Nothing ) ->
            ( { model | page = Root <| Just conflictId }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Dogroll"
    , body =
        [ case model.page of
            Conflict _ (Just takeDice) ->
                TakeDice.view
                    { increment = UserClickedIncrementDie
                    , decrement = UserClickedDecrementDie
                    , roll = UserClickedRollDice
                    }
                    takeDice

            Conflict conflict Nothing ->
                conflict
                    |> Conflict.View.view
                        { takeMoreDice = UserClickedTakeMoreDice
                        , playDie = UserClickedPlayDie
                        , raise = UserClickedRaise
                        , see = UserClickedSee
                        , fallout = UserClickedFalloutSize
                        , give = UserClickedGive
                        , restart = UserClickedRestart
                        , participate = UserClickedParticipate
                        , noop = UserClickedSomethingUnneeded
                        }

            Root maybeConflictId ->
                Root.view maybeConflictId

            LoadingConflict _ ->
                UI.pool [ UI.poolCaption "Loading…" ]
        ]
    }
