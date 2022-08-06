module Evergreen.V3.Conflict.Manager exposing (..)

import Evergreen.V3.Conflict
import Evergreen.V3.Die
import Set


type alias Id = String


type alias PendingParticipantsState = 
    { id : Id
    , side : (Maybe (Result Evergreen.V3.Conflict.Side Evergreen.V3.Conflict.Side))
    }


type alias InProgressState = 
    { id : Id
    , conflict : Evergreen.V3.Conflict.State
    , you : (Maybe Evergreen.V3.Conflict.Side)
    }


type alias FinishedState = 
    { id : Id
    , you : (Maybe Evergreen.V3.Conflict.Side)
    , followUp : (Maybe (Evergreen.V3.Conflict.Side, (Evergreen.V3.Die.Die Evergreen.V3.Die.Rolled)))
    }


type State
    = PendingParticipants PendingParticipantsState
    | InProgress InProgressState
    | Finished FinishedState


type alias Participant = 
    { id : Id
    }


type alias Model = 
    { id : Id
    , conflict : Evergreen.V3.Conflict.Conflict
    , proponent : (Maybe Participant)
    , opponent : (Maybe Participant)
    , spectators : (Set.Set Id)
    }


type Manager
    = Manager Model


type Error
    = CanNotParticipateAsBothSides
    | SideAlreadyRegistered
    | NotAParticipant
    | ConflictError Evergreen.V3.Conflict.Error