module Backend exposing (app)

import Conflict
import Conflict.Manager as Manager exposing (Manager)
import Dice
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
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
      , conflict = Manager.init "the only conflict for now"
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

                updatedConflict =
                    model.conflict
                        |> Manager.takeAction
                            (Conflict.takeDice rolled)
                            sessionId

                updatedModel =
                    { model | conflict = updatedConflict }

                cmds =
                    Cmd.batch
                        [ newSeed
                        , publishChanges updatedConflict
                        ]
            in
            ( updatedModel, cmds )

        UserWantsToParticipate side ->
            Manager.register side sessionId
                |> updateAndPublishConflict
                |> (|>) model
                |> (\( newModel, cmds ) ->
                        newModel.conflict
                            |> Manager.error
                            |> Maybe.map (always Cmd.none)
                            |> Maybe.withDefault
                                (Cmd.batch [ cmds, sendToFrontend clientId <| RegisteredAs side ])
                            |> Tuple.pair newModel
                   )

        UserWantsToPlayDie die ->
            Manager.takeAction (Conflict.play die) sessionId
                |> updateAndPublishConflict
                |> (|>) model

        UserWantsToRaise ->
            Manager.takeAction Conflict.raise sessionId
                |> updateAndPublishConflict
                |> (|>) model

        UserWantsToSee ->
            Manager.takeAction Conflict.see sessionId
                |> updateAndPublishConflict
                |> (|>) model

        UserWantsToSelectFalloutDice size ->
            Manager.takeAction (Conflict.takeFallout size) sessionId
                |> updateAndPublishConflict
                |> (|>) model

        UserWantsToGive ->
            Manager.takeAction Conflict.give sessionId
                |> updateAndPublishConflict
                |> (|>) model

        UserWantsToRestart ->
            Manager.init "restarted conflict"
                |> always
                |> updateAndPublishConflict
                |> (|>) model
                |> Tuple.mapSecond
                    (List.singleton >> (::) newSeed >> Cmd.batch)


updateAndPublishConflict : (Manager -> Manager) -> Model -> ( Model, Cmd BackendMsg )
updateAndPublishConflict transform model =
    case transform model.conflict of
        conflict ->
            ( { model | conflict = conflict }
            , conflict |> publishChanges
            )


publishChanges : Manager -> Cmd BackendMsg
publishChanges =
    Manager.conflict
        >> Conflict.state
        >> ConflictStateUpdated
        >> Lamdera.broadcast
