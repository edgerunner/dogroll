module Conflict.Manager exposing (Manager, conflict, id, init)

import Conflict exposing (Conflict)


type alias Model =
    { id : Id
    , conflict : Conflict
    }


type alias Id =
    String


type Manager
    = Manager Model


init : Id -> Manager
init id_ =
    Manager
        { id = id_
        , conflict = Conflict.start
        }


id : Manager -> Id
id (Manager model) =
    model.id


conflict : Manager -> Conflict
conflict (Manager model) =
    model.conflict
