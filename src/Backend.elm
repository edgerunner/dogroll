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
updateFromFrontend sessionId _ msg model =
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
            in
            updatedConflict
                |> handleConflictManagerUpdate
                |> with model
                |> Tuple.mapSecond
                    (List.singleton >> (::) newSeed >> Cmd.batch)

        UserWantsToParticipate side ->
            Manager.register side sessionId model.conflict
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToPlayDie die ->
            Manager.takeAction (Conflict.play die) sessionId model.conflict
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToRaise ->
            Manager.takeAction Conflict.raise sessionId model.conflict
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToSee ->
            Manager.takeAction Conflict.see sessionId model.conflict
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToSelectFalloutDice size ->
            Manager.takeAction (Conflict.takeFallout size) sessionId model.conflict
                |> handleConflictManagerUpdate
                |> with model

        UserWantsToGive ->
            Manager.takeAction Conflict.give sessionId model.conflict
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


handleConflictManagerUpdate : ( Manager, List Effect ) -> Model -> ( Model, Cmd BackendMsg )
handleConflictManagerUpdate ( manager, effects ) model =
    effects
        |> List.map
            (\effect ->
                -- tentative until we sort out the session/client id question
                case effect of
                    StateUpdate _ state ->
                        ConflictStateUpdated state |> Lamdera.broadcast

                    ParticipantUpdate _ proponent opponent ->
                        ParticipantsUpdated proponent opponent |> Lamdera.broadcast

                    ErrorResponse _ error ->
                        ErrorReported error |> Lamdera.broadcast
            )
        |> Cmd.batch
        |> Tuple.pair { model | conflict = manager }


with : a -> (a -> b) -> b
with =
    (|>)
