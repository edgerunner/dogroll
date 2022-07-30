module Conflict.Manager exposing (Error(..), Manager, addSpectator, conflict, error, id, init, opponent, proponent, register, spectators, takeAction)

import Conflict exposing (Conflict, Side)
import Set exposing (Set)


type alias Model =
    { id : Id
    , conflict : Conflict
    , proponent : Maybe Participant
    , opponent : Maybe Participant
    , error : Maybe Error
    , spectators : Set String
    }


type alias Id =
    String


type Manager
    = Manager Model


type alias Participant =
    { id : Id
    }


init : Id -> Manager
init id_ =
    Manager
        { id = id_
        , conflict = Conflict.start
        , proponent = Nothing
        , opponent = Nothing
        , error = Nothing
        , spectators = Set.empty
        }


id : Manager -> Id
id (Manager model) =
    model.id


conflict : Manager -> Conflict
conflict (Manager model) =
    model.conflict



-- UPDATERS: x -> Manager -> Manager


register : Side -> String -> Manager -> Manager
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
                    sideTakenError =
                        getSide side model
                            |> Maybe.map (always <| setError SideAlreadyRegistered)
                            |> Maybe.withDefault clearError
                in
                model
                    |> sideTakenError
                    |> updateSide side
                        (Maybe.withDefault { id = participantId } >> Just)

            else
                model |> setError CanNotParticipateAsBothSides
        )


takeAction : (Side -> Conflict -> Result Conflict.Error Conflict) -> Id -> Manager -> Manager
takeAction action participantId =
    updateModel
        (\model ->
            identifySide participantId model
                |> Result.andThen (action >> (|>) model.conflict >> Result.mapError ConflictError)
                |> Result.map (\conflict_ -> { model | conflict = conflict_ })
                |> Result.mapError (setError >> (|>) model)
                |> collapseResult
        )


addSpectator : String -> Manager -> Manager
addSpectator spectatorId =
    updateModel
        (\model -> { model | spectators = Set.insert spectatorId model.spectators })



-- GETTERS: Manager -> x


proponent : Manager -> Maybe Participant
proponent (Manager model) =
    model.proponent


opponent : Manager -> Maybe Participant
opponent (Manager model) =
    model.opponent


spectators : Manager -> Set String
spectators (Manager model) =
    model.spectators


error : Manager -> Maybe Error
error (Manager model) =
    model.error



-- ERRORS


type Error
    = CanNotParticipateAsBothSides
    | SideAlreadyRegistered
    | NotAParticipant
    | ConflictError Conflict.Error


setError : Error -> Model -> Model
setError error_ model =
    { model | error = Just error_ }


clearError : Model -> Model
clearError model =
    { model | error = Nothing }



-- HELPERS


updateModel : (Model -> Model) -> Manager -> Manager
updateModel updateFn (Manager model) =
    Manager (updateFn model)


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


collapseResult : Result a a -> a
collapseResult result =
    case result of
        Ok ok ->
            ok

        Err err ->
            err
