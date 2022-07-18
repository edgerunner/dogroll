module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dice.Pips
import Die.Size
import Random exposing (Seed)
import Setup
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , setup : Setup.Model
    }


type alias BackendModel =
    { seed : Seed
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UserClickedIncrementDie Die.Size.Size
    | UserClickedDecrementDie Die.Size.Size
    | UserClickedRollDice


type ToBackend
    = UserWantsToRollDice (Die.Size.Sizes Dice.Pips.Pips)


type BackendMsg
    = RandomGeneratedSeed Seed


type ToFrontend
    = NoOpToFrontend
