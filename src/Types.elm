module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Conflict exposing (Conflict, Side)
import Dice exposing (Dice)
import Die exposing (Die, Held, Rolled)
import Die.Size exposing (Size)
import Lamdera exposing (SessionId)
import Random exposing (Seed)
import Setup
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , setup : Setup.Model
    , conflict : Conflict.State
    , mySide : Maybe Side
    , page : Page
    }


type Page
    = Setup
    | Conflict


type alias BackendModel =
    { seed : Seed
    , conflict : Conflict
    , participants : ( SessionId, SessionId )
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


type ToBackend
    = UserWantsToRollDice (Dice Held)
    | UserWantsToParticipate
    | UserWantsToPlayDie (Die Rolled)
    | UserWantsToRaise
    | UserWantsToSee
    | UserWantsToSelectFalloutDice Size
    | UserWantsToGive
    | UserWantsToRestart


type BackendMsg
    = RandomGeneratedSeed Seed


type ToFrontend
    = ConflictStateUpdated Conflict.State
    | RegisteredAs (Maybe Side)
