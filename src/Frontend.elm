module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Conflict
import Conflict.View
import Die.Size exposing (Size(..))
import Lamdera exposing (Key, sendToBackend)
import Setup
import Types exposing (..)
import Url


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


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , setup = Setup.empty
      , conflict = Conflict.initialState
      , mySide = Nothing
      , page = Setup
      }
    , sendToBackend UserWantsToParticipate
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
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
            sendToBackend (UserWantsToRollDice model.setup)
                |> Tuple.pair { model | setup = Setup.empty, page = Conflict }

        UserClickedTakeMoreDice ->
            { model | page = Setup } |> noCmd

        UserClickedPlayDie die ->
            ( model, sendToBackend (UserWantsToPlayDie die) )

        UserClickedRaise ->
            ( model, sendToBackend UserWantsToRaise )

        UserClickedSee ->
            ( model, sendToBackend UserWantsToSee )

        UserClickedFalloutSize size ->
            ( model, sendToBackend (UserWantsToSelectFalloutDice size) )

        UserClickedRestart ->
            ( model, sendToBackend UserWantsToRestart )

        UserClickedSomethingUnneeded ->
            ( model, Cmd.none )


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        ConflictStateUpdated newConflictState ->
            let
                straighten =
                    if (model.mySide |> Debug.log "my side") == Just Conflict.opponent then
                        Conflict.mirror

                    else
                        identity
            in
            ( { model | conflict = straighten newConflictState }, Cmd.none )

        RegisteredAs maybeSide ->
            ( { model | mySide = maybeSide }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Dogroll"
    , body =
        [ case model.page of
            Setup ->
                Setup.view
                    { increment = UserClickedIncrementDie
                    , decrement = UserClickedDecrementDie
                    , roll = UserClickedRollDice
                    }
                    model.setup

            Conflict ->
                model.conflict
                    |> Conflict.View.view
                        { takeMoreDice = UserClickedTakeMoreDice
                        , playDie = UserClickedPlayDie
                        , raise = UserClickedRaise
                        , see = UserClickedSee
                        , fallout = UserClickedFalloutSize
                        , restart = UserClickedRestart
                        , noop = UserClickedSomethingUnneeded
                        }
        ]
    }
