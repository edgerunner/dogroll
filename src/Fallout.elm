module Fallout exposing (Fallout, Outcome(..), State(..))

import Conflict exposing (Conflict)
import Dice exposing (Dice)
import Die exposing (Held, Rolled)


type Fallout
    = Fallout (List Event)


type Event
    = TookDice (Dice Held)
    | RolledFallout (Dice Rolled)
    | RolledPatientBody (Dice Rolled)
    | RolledHealerAcuity (Dice Rolled)
    | RolledDemonicInfluence (Dice Rolled)
    | StartedConflict
    | EndedConflict Conflict


type State
    = Pending (Dice Held)
    | ExpectingPatientBody (Dice Rolled)
    | ExpectingDice
        { fallout : Dice Held
        , patientBody : Maybe (Dice Held)
        , healerAcuity : Maybe (Dice Held)
        , demonicInfluence : Maybe (Dice Held)
        }
    | InConflict Conflict
    | Concluded Bool Outcome


type Outcome
    = ShortTerm
    | LongTerm
    | DoubleLongTerm
    | Dying
