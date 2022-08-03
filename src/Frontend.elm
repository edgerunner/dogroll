module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Conflict.Manager
import Conflict.View
import Die.Size exposing (Size(..))
import Lamdera exposing (Key, sendToBackend)
import Root
import Setup
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
      , setup = Setup.empty
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
            case model.conflict of
                Conflict.Manager.NotConnected ->
                    ""

                Conflict.Manager.PendingParticipants pp ->
                    pp.id

                Conflict.Manager.InProgress ip ->
                    ip.id

                Conflict.Manager.Finished f ->
                    f.id
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
            { model | setup = Setup.increment size model.setup } |> noCmd

        UserClickedDecrementDie size ->
            { model | setup = Setup.decrement size model.setup } |> noCmd

        UserClickedRollDice ->
            sendToBackend (ForConflict conflictId <| UserWantsToRollDice model.setup)
                |> Tuple.pair { model | setup = Setup.empty, page = Conflict conflictId }

        UserClickedTakeMoreDice ->
            { model | page = Setup conflictId } |> noCmd

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
    case msg of
        StateUpdated newConflictState ->
            ( { model | conflict = newConflictState }, Cmd.none )

        ErrorReported _ ->
            -- TODO: show error
            ( model, Cmd.none )

        ConflictNotFound _ ->
            -- TODO: show error
            ( model, Cmd.none )

        RandomConflictIdGenerated conflictId ->
            ( { model | page = Root <| Just conflictId }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Dogroll"
    , body =
        [ case model.page of
            Setup _ ->
                Setup.view
                    { increment = UserClickedIncrementDie
                    , decrement = UserClickedDecrementDie
                    , roll = UserClickedRollDice
                    }
                    model.setup

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
