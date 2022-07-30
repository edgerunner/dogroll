module Conflict.Manager exposing (Effect(..), Error(..), Manager, addSpectator, conflict, id, init, opponent, proponent, register, spectators, subscribers, takeAction)

import Conflict exposing (Conflict, Side)
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
    = StateUpdate (List Id) Conflict.State
    | ParticipantUpdate (List Id) Bool Bool
    | ErrorResponse Id Error


type Error
    = CanNotParticipateAsBothSides
    | SideAlreadyRegistered
    | NotAParticipant
    | ConflictError Conflict.Error


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

                    sideFilled =
                        Maybe.map (always True)
                            >> Maybe.withDefault False

                    participantUpdate =
                        ParticipantUpdate
                            (getSubscribers newModel)
                            (sideFilled newModel.proponent)
                            (sideFilled newModel.opponent)
                in
                getSide side model
                    |> Maybe.map (always ( model, [ ErrorResponse participantId SideAlreadyRegistered ] ))
                    |> Maybe.withDefault ( newModel, [ participantUpdate ] )

            else
                ( model, [ ErrorResponse participantId CanNotParticipateAsBothSides ] )
        )


takeAction : (Side -> Conflict -> Result Conflict.Error Conflict) -> Id -> Manager -> ( Manager, List Effect )
takeAction action participantId =
    updateModel
        (\model ->
            identifySide participantId model
                |> Result.andThen (action >> (|>) model.conflict >> Result.mapError ConflictError)
                |> Result.map
                    (\conflict_ ->
                        ( { model | conflict = conflict_ }
                        , [ StateUpdate (getSubscribers model) (Conflict.state conflict_) ]
                        )
                    )
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
