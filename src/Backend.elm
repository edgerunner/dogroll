module Backend exposing (..)

import Dice
import Lamdera exposing (ClientId, SessionId)
import Random
import Setup
import Types exposing (..)


type alias Model =
    BackendModel


app :
    { init : ( Model, Cmd BackendMsg )
    , update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
    , subscriptions : Model -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"
      , seed = Random.initialSeed 0
      }
    , Random.generate RandomGeneratedSeed Random.independentSeed
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        RandomGeneratedSeed seed ->
            ( { model | seed = seed }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        UserWantsToRollDice sizes ->
            let
                _ =
                    Setup.roll sizes model.seed
                        |> Dice.faces
                        |> Debug.log "Rolled"
            in
            ( model, Cmd.none )
