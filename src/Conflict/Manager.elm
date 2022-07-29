module Conflict.Manager exposing (Manager, conflict, id, init, opponent, proponent, registerOpponent, registerProponent)

import Conflict exposing (Conflict)


type alias Model =
    { id : Id
    , conflict : Conflict
    , proponent : Maybe Participant
    , opponent : Maybe Participant
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
        }


id : Manager -> Id
id (Manager model) =
    model.id


conflict : Manager -> Conflict
conflict (Manager model) =
    model.conflict


registerProponent : String -> Manager -> Manager
registerProponent proponentId =
    updateModel
        (\model ->
            if
                model.opponent
                    |> Maybe.map (.id >> (/=) proponentId)
                    |> Maybe.withDefault True
            then
                { model
                    | proponent =
                        model.proponent
                            |> Maybe.withDefault { id = proponentId }
                            |> Just
                }

            else
                model
        )


registerOpponent : String -> Manager -> Manager
registerOpponent opponentId =
    updateModel
        (\model ->
            if
                model.proponent
                    |> Maybe.map (.id >> (/=) opponentId)
                    |> Maybe.withDefault True
            then
                { model
                    | opponent =
                        model.opponent
                            |> Maybe.withDefault { id = opponentId }
                            |> Just
                }

            else
                model
        )


proponent : Manager -> Maybe Participant
proponent (Manager model) =
    model.proponent


opponent : Manager -> Maybe Participant
opponent (Manager model) =
    model.opponent



-- HELPERS


updateModel : (Model -> Model) -> Manager -> Manager
updateModel updateFn (Manager model) =
    Manager (updateFn model)
