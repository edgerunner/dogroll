module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Conflict exposing (Side)
import Conflict.Manager
import Dice exposing (Dice)
import Die exposing (Die, Held, Rolled)
import Die.Size exposing (Size)
import Random exposing (Seed)
import Setup
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , setup : Setup.Model
    , conflict : Conflict.State
    , mySide : Maybe Side
    , sides : ( Bool, Bool )
    , page : Page
    }


type Page
    = Setup
    | Conflict


type alias BackendModel =
    { seed : Seed
    , conflict : Conflict.Manager.Manager
    }


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
    = UserWantsToParticipate Side
    | UserWantsToRollDice (Dice Held)
    | UserWantsToPlayDie (Die Rolled)
    | UserWantsToRaise
    | UserWantsToSee
    | UserWantsToSelectFalloutDice Size
    | UserWantsToGive
    | UserWantsToRestart
    | ClientInitialized


type BackendMsg
    = RandomGeneratedSeed Seed


type ToFrontend
    = ConflictStateUpdated Conflict.State
    | RegisteredAs Side
    | ParticipantsUpdated Bool Bool
    | ErrorReported Conflict.Manager.Error
