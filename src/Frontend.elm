module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Conflict.Manager
import Conflict.View
import Die.Size exposing (Size(..))
import Lamdera exposing (Key, sendToBackend)
import Root
import TakeDice
import Types exposing (..)
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
    ( { key = key
      , takeDice = TakeDice.empty
      , conflict = Conflict.Manager.initialState
      , page =
            Url.Parser.string
                |> Url.Parser.map Conflict
                |> Url.Parser.parse
                |> (|>) url
                |> Maybe.withDefault (Root Nothing)
      }
    , url
        |> Url.Parser.parse Url.Parser.string
        |> Maybe.map (ClientInitialized >> Lamdera.sendToBackend)
        |> Maybe.withDefault
            (Lamdera.sendToBackend ClientRequestedRandomConflictId)
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        conflictId =
            Conflict.Manager.stateId model.conflict
    in
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.load (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        UserClickedIncrementDie size ->
            { model | takeDice = TakeDice.increment size model.takeDice } |> noCmd

        UserClickedDecrementDie size ->
            { model | takeDice = TakeDice.decrement size model.takeDice } |> noCmd

        UserClickedRollDice ->
            sendToBackend (ForConflict conflictId <| UserWantsToRollDice model.takeDice)
                |> Tuple.pair { model | takeDice = TakeDice.empty, page = Conflict conflictId }

        UserClickedTakeMoreDice ->
            { model | page = TakeDice conflictId } |> noCmd

        UserClickedPlayDie die ->
            ( model, sendToBackend (ForConflict conflictId <| UserWantsToPlayDie die) )

        UserClickedRaise ->
            ( model, sendToBackend (ForConflict conflictId <| UserWantsToRaise) )

        UserClickedSee ->
            ( model, sendToBackend (ForConflict conflictId <| UserWantsToSee) )

        UserClickedFalloutSize size ->
            ( model, sendToBackend (ForConflict conflictId <| UserWantsToSelectFalloutDice size) )

        UserClickedGive ->
            ( model, sendToBackend (ForConflict conflictId <| UserWantsToGive) )

        UserClickedRestart ->
            ( model, sendToBackend (ForConflict conflictId <| UserWantsToFollowUp) )

        UserClickedParticipate side ->
            ( model, sendToBackend (ForConflict conflictId <| UserWantsToParticipate side) )

        UserClickedSomethingUnneeded ->
            ( model, Cmd.none )


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model.page ) of
        ( StateUpdated newConflictState, Conflict conflictId ) ->
            if Conflict.Manager.stateId newConflictState == conflictId then
                ( { model | conflict = newConflictState }, Cmd.none )

            else
                ( model, Cmd.none )

        ( StateUpdated newConflictState, TakeDice conflictId ) ->
            if Conflict.Manager.stateId newConflictState == conflictId then
                ( { model | conflict = newConflictState }, Cmd.none )

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
            TakeDice _ ->
                TakeDice.view
                    { increment = UserClickedIncrementDie
                    , decrement = UserClickedDecrementDie
                    , roll = UserClickedRollDice
                    }
                    model.takeDice

            Conflict _ ->
                model.conflict
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
        ]
    }
