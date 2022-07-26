module Backend exposing (..)

import Conflict
import Dice
import Lamdera exposing (ClientId, SessionId)
import Random
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
    ( { seed = Random.initialSeed 0
      , conflict = Conflict.start
      , participants = ( "", "" )
      }
    , newSeed
    )


newSeed : Cmd BackendMsg
newSeed =
    Random.generate RandomGeneratedSeed Random.independentSeed


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        RandomGeneratedSeed seed ->
            ( { model | seed = seed }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId _ msg model =
    let
        participant =
            case
                model.participants
                    |> Tuple.mapBoth ((==) sessionId) ((==) sessionId)
            of
                ( True, _ ) ->
                    Just Conflict.proponent

                ( _, True ) ->
                    Just Conflict.opponent

                _ ->
                    Nothing
    in
    case msg of
        UserWantsToRollDice dice ->
            let
                rolled =
                    Dice.roll model.seed dice

                conflictUpdate =
                    participant
                        |> Maybe.map Conflict.takeDice
                        |> Maybe.withDefault (always Ok)

                updatedConflict =
                    conflictUpdate rolled model.conflict
                        |> Result.withDefault model.conflict

                updatedModel =
                    { model | conflict = updatedConflict }

                updatedState =
                    Conflict.state updatedConflict

                cmds =
                    Cmd.batch
                        [ newSeed
                        , Lamdera.broadcast (ConflictStateUpdated updatedState)
                        ]
            in
            ( updatedModel, cmds )

        UserWantsToParticipate ->
            case model.participants of
                ( "", "" ) ->
                    ( { model | participants = ( sessionId, "" ) }, Cmd.none )

                ( proponentId, "" ) ->
                    if sessionId /= proponentId then
                        ( { model | participants = ( proponentId, sessionId ) }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UserWantsToPlayDie die ->
            case participant of
                Just side ->
                    case Conflict.play side die model.conflict of
                        Ok conflict ->
                            ( { model | conflict = conflict }
                            , conflict
                                |> Conflict.state
                                |> ConflictStateUpdated
                                |> Lamdera.broadcast
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UserWantsToRaise ->
            case participant of
                Just side ->
                    case Conflict.raise side model.conflict of
                        Ok conflict ->
                            ( { model | conflict = conflict }
                            , conflict
                                |> Conflict.state
                                |> ConflictStateUpdated
                                |> Lamdera.broadcast
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
