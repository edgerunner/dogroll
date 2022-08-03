module Backend exposing (app)

import Conflict
import Conflict.Manager as Manager exposing (Effect(..), Manager)
import Dice
import Dict
import Lamdera exposing (ClientId, SessionId)
import Random
import Random.Words
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
      , conflicts = Dict.empty
      }
    , newSeed
    )


newSeed : Cmd BackendMsg
newSeed =
    Random.generate RandomGeneratedSeed Random.independentSeed


conflictNotFound : ClientId -> Id -> Cmd BackendMsg
conflictNotFound clientId conflictId =
    Lamdera.sendToFrontend
        clientId
        (ConflictNotFound conflictId)


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        RandomGeneratedSeed seed ->
            ( { model | seed = seed }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ClientRequestedRandomConflictId ->
            Random.step Random.Words.generator model.seed
                |> Tuple.first
                |> RandomConflictIdGenerated
                |> Lamdera.sendToFrontend clientId
                |> List.singleton
                |> (::) newSeed
                |> Cmd.batch
                |> Tuple.pair model

        ClientInitialized conflictId ->
            model.conflicts
                |> Dict.get conflictId
                |> Maybe.withDefault (Manager.init conflictId)
                |> Manager.addSpectator sessionId
                |> handleConflictManagerUpdate
                |> with model

        ForConflict conflictId userWants ->
            let
                currentConflict =
                    model.conflicts
                        |> Dict.get conflictId

                defaultToConflictNotFound =
                    Maybe.withDefault
                        ( model
                        , conflictNotFound clientId conflictId
                        )

                withCurrentConflict transform =
                    currentConflict
                        |> Maybe.map
                            (transform
                                >> handleConflictManagerUpdate
                                >> with model
                            )
                        |> defaultToConflictNotFound

                refreshSeed =
                    Tuple.mapSecond
                        (List.singleton >> (::) newSeed >> Cmd.batch)
            in
            case userWants of
                UserWantsToRollDice dice ->
                    let
                        rolled =
                            Dice.roll model.seed dice
                    in
                    withCurrentConflict
                        (Manager.takeAction
                            (Conflict.takeDice rolled)
                            sessionId
                        )
                        |> refreshSeed

                UserWantsToParticipate side ->
                    withCurrentConflict
                        (Manager.register side sessionId)

                UserWantsToPlayDie die ->
                    withCurrentConflict
                        (Manager.takeAction (Conflict.play die) sessionId)

                UserWantsToRaise ->
                    withCurrentConflict
                        (Manager.takeAction Conflict.raise sessionId)

                UserWantsToSee ->
                    withCurrentConflict
                        (Manager.takeAction Conflict.see sessionId)

                UserWantsToSelectFalloutDice size ->
                    withCurrentConflict
                        (Manager.takeAction (Conflict.takeFallout size) sessionId)

                UserWantsToGive ->
                    withCurrentConflict
                        (Manager.takeAction Conflict.give sessionId)

                UserWantsToFollowUp ->
                    withCurrentConflict
                        (Manager.followUp sessionId)


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
        |> Tuple.pair
            { model
                | conflicts =
                    model.conflicts
                        |> Dict.insert
                            (Manager.id manager)
                            manager
            }


sendAllToFrontends : List SessionId -> ToFrontend -> Cmd BackendMsg
sendAllToFrontends ids msg =
    List.map (Lamdera.sendToFrontend >> with msg) ids
        |> Cmd.batch


with : a -> (a -> b) -> b
with =
    (|>)
