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
    , message : String
    , setup : Setup.Model
    }


type alias BackendModel =
    { message : String
    , seed : Seed
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | UserClickedIncrementDie Die.Size.Size
    | UserClickedDecrementDie Die.Size.Size
    | UserClickedRollDice


type ToBackend
    = NoOpToBackend
    | UserWantsToRollDice (Die.Size.Sizes Dice.Pips.Pips)


type BackendMsg
    = NoOpBackendMsg
    | RandomGeneratedSeed Seed


type ToFrontend
    = NoOpToFrontend
