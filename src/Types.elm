module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Conflict exposing (Conflict)
import Dice exposing (Dice)
import Die.Size
import Lamdera exposing (SessionId)
import Random exposing (Seed)
import Setup
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , setup : Setup.Model
    , conflict : Conflict.State
    }


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


type ToBackend
    = UserWantsToRollDice Dice
    | UserWantsToParticipate


type BackendMsg
    = RandomGeneratedSeed Seed


type ToFrontend
    = ConflictStateUpdated Conflict.State
