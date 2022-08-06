module Evergreen.V3.Conflict exposing (..)

import Evergreen.V3.Dice
import Evergreen.V3.Die
import Evergreen.V3.Die.Size
import Evergreen.V3.Pips


type Side
    = Proponent
    | Opponent


type alias Dice = (Evergreen.V3.Dice.Dice Evergreen.V3.Die.Rolled)


type alias Die = (Evergreen.V3.Die.Die Evergreen.V3.Die.Rolled)


type alias Player = 
    { pool : Dice
    , fallout : (Evergreen.V3.Dice.Dice Evergreen.V3.Die.Held)
    , bestDie : (Maybe Die)
    }


type See
    = LoseTheStakes
    | ReverseTheBlow Die
    | BlockOrDodge Die Die
    | TakeTheBlow Die Die Die (List Die)


type Raise
    = PendingTwoDice
    | PendingOneDie Die
    | ReadyToRaise Die Die
    | RaisedWith Die Die See
    | PendingFallout Evergreen.V3.Pips.Pips
    | GivenUp (Maybe Die)


type alias State = 
    { proponent : Player
    , opponent : Player
    , raise : Raise
    , go : Side
    }


type Event
    = TookDice Dice
    | Played Die
    | Raised
    | Seen
    | TookFallout Evergreen.V3.Die.Size.Size
    | Gave


type Conflict
    = Conflict (List (Side, Event))


type Error
    = DieNotInPool
    | RaiseWithTwoDice
    | NotYourTurn
    | NotEnoughToSee
    | MustTakeFallout
    | NoPendingFallout