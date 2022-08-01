module Backend exposing (app)

import Conflict
import Conflict.Manager as Manager exposing (Effect(..), Manager)
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
      , conflictManager = Manager.init "the only conflict for now"
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

                updatedConflict =
                    model.conflictManager
                        |> Manager.takeAction
                            (Conflict.takeDice rolled)
                            sessionId
            in
            updatedConflict
                |> handleConflictManagerUpdate
                |> with model
                |> Tuple.mapSecond
                    (List.singleton >> (::) newSeed >> Cmd.batch)

        UserWantsToParticipate side ->
            Manager.register side sessionId model.conflictManager
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToPlayDie die ->
            Manager.takeAction (Conflict.play die) sessionId model.conflictManager
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToRaise ->
            Manager.takeAction Conflict.raise sessionId model.conflictManager
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToSee ->
            Manager.takeAction Conflict.see sessionId model.conflictManager
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToSelectFalloutDice size ->
            Manager.takeAction (Conflict.takeFallout size) sessionId model.conflictManager
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToGive ->
            Manager.takeAction Conflict.give sessionId model.conflictManager
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToRestart ->
            Manager.init "restarted conflict"
                |> Tuple.pair
                |> with []
                |> handleConflictManagerUpdate
                |> with model
                |> Tuple.mapSecond
                    (List.singleton >> (::) newSeed >> Cmd.batch)

        ClientInitialized ->
            model.conflictManager
                |> Manager.addSpectator sessionId
                |> handleConflictManagerUpdate
                |> with model


handleConflictManagerUpdate : ( Manager, List Effect ) -> Model -> ( Model, Cmd BackendMsg )
handleConflictManagerUpdate ( manager, effects ) model =
    effects
        |> List.map
            (\effect ->
                case effect of
                    StateUpdate ids state ->
                        StateUpdated state |> sendAllToFrontends ids

                    ErrorResponse id error ->
                        ErrorReported error |> Lamdera.sendToFrontend id
            )
        |> Cmd.batch
        |> Tuple.pair { model | conflictManager = manager }


sendAllToFrontends : List SessionId -> ToFrontend -> Cmd BackendMsg
sendAllToFrontends ids msg =
    List.map (Lamdera.sendToFrontend >> with msg) ids
        |> Cmd.batch


with : a -> (a -> b) -> b
with =
    (|>)
