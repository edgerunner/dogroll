module Conflict.Manager exposing (Manager, id, init)


type alias Model =
    { id : Id
    }


type alias Id =
    String


type Manager
    = Manager Model


init : Id -> Manager
init id_ =
    Manager { id = id_ }


id : Manager -> Id
id (Manager model) =
    model.id
