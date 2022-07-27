module Backend exposing (..)

import Conflict exposing (Conflict, Side)
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
updateFromFrontend sessionId clientId msg model =
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

                cmds =
                    Cmd.batch
                        [ newSeed
                        , publishChanges updatedConflict
                        ]
            in
            ( updatedModel, cmds )

        UserWantsToParticipate ->
            case model.participants of
                ( "", "" ) ->
                    ( { model | participants = ( sessionId, "" ) }
                    , Lamdera.sendToFrontend clientId (RegisteredAs (Just Conflict.proponent))
                    )

                ( proponentId, "" ) ->
                    if sessionId /= proponentId then
                        ( { model | participants = ( proponentId, sessionId ) }
                        , Lamdera.sendToFrontend clientId (RegisteredAs (Just Conflict.opponent))
                        )

                    else
                        ( model, Lamdera.sendToFrontend clientId (RegisteredAs (Just Conflict.proponent)) )

                ( proponentId, opponentId ) ->
                    (if sessionId == proponentId then
                        Just Conflict.proponent

                     else if sessionId == opponentId then
                        Just Conflict.opponent

                     else
                        Nothing
                    )
                        |> RegisteredAs
                        |> Lamdera.sendToFrontend clientId
                        |> Tuple.pair model

        UserWantsToPlayDie die ->
            withParticipant sessionId
                (\side ->
                    case Conflict.play side die model.conflict of
                        Ok conflict ->
                            ( { model | conflict = conflict }
                            , conflict |> publishChanges
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
                            , conflict |> publishChanges
                            )

                        Err _ ->
                            ( model, Cmd.none )
                )
                model

        UserWantsToSee ->
            withParticipant sessionId
                (\side ->
                    case Conflict.see side model.conflict of
                        Ok conflict ->
                            ( { model | conflict = conflict }
                            , conflict |> publishChanges
                            )

                        Err _ ->
                            ( model, Cmd.none )
                )
                model

        UserWantsToSelectFalloutDice size ->
            withParticipant sessionId
                (\side ->
                    case Conflict.takeFallout side size model.conflict of
                        Ok conflict ->
                            ( { model | conflict = conflict }
                            , conflict |> publishChanges
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


publishChanges : Conflict -> Cmd BackendMsg
publishChanges conflict =
    conflict
        |> Conflict.state
        |> ConflictStateUpdated
        |> Lamdera.broadcast
