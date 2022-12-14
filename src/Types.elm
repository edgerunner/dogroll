module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Conflict exposing (Side)
import Conflict.Manager exposing (Manager, State)
import Dice exposing (Dice)
import Dict exposing (Dict)
import Die exposing (Die, Held, Rolled)
import Die.Size exposing (Size)
import Random exposing (Seed)
import TakeDice
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , page : Page
    }


type Page
    = Root (Maybe Id)
    | LoadingConflict Id
    | Conflict State (Maybe TakeDice.Model)


type alias BackendModel =
    { seed : Seed
    , conflicts : Dict Id Manager
    }


type alias Id =
    String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UserClickedIncrementDie Die.Size.Size
    | UserClickedDecrementDie Die.Size.Size
    | UserClickedRollDice
    | UserClickedTakeMoreDice
    | UserClickedPlayDie (Die Rolled)
    | UserClickedSomethingUnneeded
    | UserClickedRaise
    | UserClickedSee
    | UserClickedFalloutSize Die.Size.Size
    | UserClickedRestart
    | UserClickedGive
    | UserClickedParticipate Side


type ToBackend
    = ForConflict Id UserWantsTo
    | ClientInitialized Id
    | ClientRequestedRandomConflictId


type UserWantsTo
    = UserWantsToParticipate Side
    | UserWantsToRollDice (Dice Held)
    | UserWantsToPlayDie (Die Rolled)
    | UserWantsToRaise
    | UserWantsToSee
    | UserWantsToSelectFalloutDice Size
    | UserWantsToGive
    | UserWantsToFollowUp


type BackendMsg
    = RandomGeneratedSeed Seed


type ToFrontend
    = StateUpdated Conflict.Manager.State
    | ErrorReported Conflict.Manager.Error
    | ConflictNotFound Id
    | RandomConflictIdGenerated Id
