module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Conflict exposing (Conflict)
import Dice exposing (Dice)
import Die.Size
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
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UserClickedIncrementDie Die.Size.Size
    | UserClickedDecrementDie Die.Size.Size
    | UserClickedRollDice


type ToBackend
    = UserWantsToRollDice Dice


type BackendMsg
    = RandomGeneratedSeed Seed


type ToFrontend
    = ConflictStateUpdated Conflict.State
