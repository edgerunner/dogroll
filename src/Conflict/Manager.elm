module Conflict.Manager exposing (Effect(..), Error(..), Manager, State(..), addSpectator, conflict, id, init, opponent, proponent, register, spectators, subscribers, takeAction)

import Conflict exposing (Conflict, Side)
import Die exposing (Die, Rolled)
import Set exposing (Set)


type alias Model =
    { id : Id
    , conflict : Conflict
    , proponent : Maybe Participant
    , opponent : Maybe Participant
    , spectators : Set Id
    }


type alias Id =
    String


type Manager
    = Manager Model


type alias Participant =
    { id : Id
    }


type Effect
    = StateUpdate (List Id) State
    | ErrorResponse Id Error


type Error
    = CanNotParticipateAsBothSides
    | SideAlreadyRegistered
    | NotAParticipant
    | ConflictError Conflict.Error


type State
    = PendingParticipants PendingParticipantsModel
    | InProgress InProgressModel
    | Finished FinishedModel


type alias PendingParticipantsModel =
    { id : Id

    {- side is the tentative state of the pickings
       from the perspective of a spectator
       Nothing : no sides are picked
       Just (Ok side) : You picked that side
       Just (Err side) : Someone else picked that side
       In any case, the state should change to InProgress
       once both sides are picked
    -}
    , side : Maybe (Result Side Side)
    }


type alias InProgressModel =
    { id : Id
    , conflict : Conflict.State
    , you : Maybe Conflict.Side
    }


type alias FinishedModel =
    { id : Id
    , you : Maybe Conflict.Side
    , followUp : Maybe ( Conflict.Side, Die Rolled )
    }


init : Id -> Manager
init id_ =
    Manager
        { id = id_
        , conflict = Conflict.start
        , proponent = Nothing
        , opponent = Nothing
        , spectators = Set.empty
        }



-- UPDATERS: x -> Manager -> (Manager, List Effect)


register : Side -> Id -> Manager -> ( Manager, List Effect )
register side participantId =
    updateModel
        (\model ->
            if
                model
                    |> (side |> Conflict.otherSide |> getSide)
                    |> Maybe.map (.id >> (/=) participantId)
                    |> Maybe.withDefault True
            then
                let
                    newModel =
                        model
                            |> updateSide side
                                (Maybe.withDefault { id = participantId } >> Just)
                in
                getSide side model
                    |> Maybe.map (always ( model, [ ErrorResponse participantId SideAlreadyRegistered ] ))
                    |> Maybe.withDefault ( newModel, getStateUpdates newModel )

            else
                ( model, [ ErrorResponse participantId CanNotParticipateAsBothSides ] )
        )


takeAction : (Side -> Conflict -> Result Conflict.Error Conflict) -> Id -> Manager -> ( Manager, List Effect )
takeAction action participantId =
    updateModel
        (\model ->
            identifySide participantId model
                |> Result.andThen (action >> (|>) model.conflict >> Result.mapError ConflictError)
                |> Result.map (\conflict_ -> { model | conflict = conflict_ })
                |> Result.map (\newModel -> ( newModel, getStateUpdates newModel ))
                |> Result.mapError (ErrorResponse participantId >> List.singleton >> Tuple.pair model)
                |> collapseResult
        )


addSpectator : Id -> Manager -> ( Manager, List Effect )
addSpectator spectatorId =
    updateModel
        (\model -> ( { model | spectators = Set.insert spectatorId model.spectators }, [] ))



-- GETTERS: Manager -> x


id : Manager -> Id
id (Manager model) =
    model.id


conflict : Manager -> Conflict
conflict (Manager model) =
    model.conflict


proponent : Manager -> Maybe Participant
proponent (Manager model) =
    model.proponent


opponent : Manager -> Maybe Participant
opponent (Manager model) =
    model.opponent


spectators : Manager -> Set Id
spectators (Manager model) =
    model.spectators


subscribers : Manager -> List Id
subscribers (Manager model) =
    getSubscribers model



-- HELPERS


updateModel : (Model -> ( Model, List Effect )) -> Manager -> ( Manager, List Effect )
updateModel updateFn (Manager model) =
    model
        |> updateFn
        |> Tuple.mapFirst Manager


updateSide : Side -> (Maybe Participant -> Maybe Participant) -> Model -> Model
updateSide side fn model =
    case side of
        Conflict.Proponent ->
            { model | proponent = fn model.proponent }

        Conflict.Opponent ->
            { model | opponent = fn model.opponent }


getSide : Side -> Model -> Maybe Participant
getSide side =
    case side of
        Conflict.Proponent ->
            .proponent

        Conflict.Opponent ->
            .opponent


identifySide : Id -> Model -> Result Error Side
identifySide participantId model =
    if Maybe.map (.id >> (==) participantId) model.proponent == Just True then
        Ok Conflict.Proponent

    else if Maybe.map (.id >> (==) participantId) model.opponent == Just True then
        Ok Conflict.Opponent

    else
        Err NotAParticipant


getSubscribers : Model -> List Id
getSubscribers model =
    model.spectators
        |> (model.proponent |> Maybe.map (.id >> Set.insert) |> Maybe.withDefault identity)
        |> (model.opponent |> Maybe.map (.id >> Set.insert) |> Maybe.withDefault identity)
        |> Set.toList


collapseResult : Result a a -> a
collapseResult result =
    case result of
        Ok ok ->
            ok

        Err err ->
            err


getStateUpdates : Model -> List Effect
getStateUpdates model =
    case ( model.proponent, model.opponent, model.conflict |> Conflict.state |> .raise ) of
        ( Just pro, Just opp, Conflict.GivenUp maybeDie ) ->
            getFinishedStateUpdates pro opp maybeDie model

        ( Just pro, Just opp, _ ) ->
            getInProgressStateUpdates pro opp model

        ( Just pro, Nothing, _ ) ->
            getPendingParticipantsStateUpdates
                (Just ( Conflict.proponent, pro ))
                model

        ( Nothing, Just opp, _ ) ->
            getPendingParticipantsStateUpdates
                (Just ( Conflict.opponent, opp ))
                model

        ( Nothing, Nothing, _ ) ->
            getPendingParticipantsStateUpdates Nothing model


getFinishedStateUpdates : Participant -> Participant -> Maybe (Die Rolled) -> Model -> List Effect
getFinishedStateUpdates pro opp followUp model =
    let
        common =
            { id = model.id
            , you = Nothing
            , followUp =
                followUp
                    |> Maybe.map
                        (model.conflict
                            |> Conflict.state
                            |> .go
                            |> Tuple.pair
                        )
            }

        spectatorUpdates =
            Finished common
                |> StateUpdate (model.spectators |> Set.toList)
                |> List.singleton

        participantUpdate side participant =
            Finished { common | you = Just side }
                |> StateUpdate [ participant.id ]
                |> (::)
    in
    spectatorUpdates
        |> participantUpdate Conflict.Proponent pro
        |> participantUpdate Conflict.Opponent opp


getInProgressStateUpdates : Participant -> Participant -> Model -> List Effect
getInProgressStateUpdates pro opp model =
    let
        common =
            { id = model.id
            , conflict = model.conflict |> Conflict.state
            , you = Nothing
            }

        spectatorUpdates =
            InProgress common
                |> StateUpdate (model.spectators |> Set.toList)
                |> List.singleton

        participantUpdate side participant =
            InProgress { common | you = Just side }
                |> StateUpdate [ participant.id ]
                |> (::)
    in
    spectatorUpdates
        |> participantUpdate Conflict.Proponent pro
        |> participantUpdate Conflict.Opponent opp


getPendingParticipantsStateUpdates : Maybe ( Side, Participant ) -> Model -> List Effect
getPendingParticipantsStateUpdates registered model =
    let
        spectatorUpdates =
            { id = model.id
            , side = registered |> Maybe.map (Tuple.first >> Err)
            }
                |> PendingParticipants
                |> StateUpdate (model.spectators |> Set.toList)
                |> List.singleton

        participantUpdate =
            registered
                |> Maybe.map
                    (\( side, participant ) ->
                        { id = model.id
                        , side = Just (Ok side)
                        }
                            |> PendingParticipants
                            |> StateUpdate [ participant.id ]
                            |> (::)
                    )
                |> Maybe.withDefault identity
    in
    spectatorUpdates |> participantUpdate
