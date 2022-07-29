module Conflict.Manager exposing (Error(..), Manager, conflict, error, id, init, opponent, proponent, register)

import Conflict exposing (Conflict, Side)


type alias Model =
    { id : Id
    , conflict : Conflict
    , proponent : Maybe Participant
    , opponent : Maybe Participant
    , error : Maybe Error
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
        }


id : Manager -> Id
id (Manager model) =
    model.id


conflict : Manager -> Conflict
conflict (Manager model) =
    model.conflict


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


proponent : Manager -> Maybe Participant
proponent (Manager model) =
    model.proponent


opponent : Manager -> Maybe Participant
opponent (Manager model) =
    model.opponent



-- ERRORS


type Error
    = CanNotParticipateAsBothSides
    | SideAlreadyRegistered


error : Manager -> Maybe Error
error (Manager model) =
    model.error


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
