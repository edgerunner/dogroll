module Backend exposing (..)

import Conflict exposing (Side)
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
    case msg of
        UserWantsToRollDice dice ->
            let
                rolled =
                    Dice.roll model.seed dice

                conflictUpdate =
                    identifyParticipant sessionId model.participants
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
            withParticipant sessionId
                (\side ->
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
                )
                model

        UserWantsToRaise ->
            withParticipant sessionId
                (\side ->
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
                )
                model


withParticipant : SessionId -> (Side -> ( Model, Cmd BackendMsg )) -> Model -> ( Model, Cmd BackendMsg )
withParticipant sessionId transform model =
    identifyParticipant sessionId model.participants
        |> Maybe.map transform
        |> Maybe.withDefault ( model, Cmd.none )


identifyParticipant : SessionId -> ( SessionId, SessionId ) -> Maybe Side
identifyParticipant sessionId ( proponentSessionId, opponentSessionId ) =
    if sessionId == proponentSessionId then
        Just Conflict.proponent

    else if sessionId == opponentSessionId then
        Just Conflict.opponent

    else
        Nothing
