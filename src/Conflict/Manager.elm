module Conflict.Manager exposing (Manager, conflict, id, init, proponent, registerProponent)

import Conflict exposing (Conflict)


type alias Model =
    { id : Id
    , conflict : Conflict
    , proponent : Maybe Participant
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
            { model
                | proponent =
                    model.proponent
                        |> Maybe.withDefault { id = proponentId }
                        |> Just
            }
        )


proponent : Manager -> Maybe Participant
proponent (Manager model) =
    model.proponent



-- HELPERS


updateModel : (Model -> Model) -> Manager -> Manager
updateModel updateFn (Manager model) =
    Manager (updateFn model)
