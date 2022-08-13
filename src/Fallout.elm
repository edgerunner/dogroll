module Fallout exposing (Fallout, Outcome(..), State(..), init, roll, state, takeDice)

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


init : Fallout
init =
    Fallout []


takeDice : Dice Held -> Fallout -> Result () Fallout
takeDice dice fallout =
    state fallout
        |> (\current ->
                case current of
                    Pending _ ->
                        fallout |> push (TookDice dice) |> Ok

                    _ ->
                        Err ()
           )


roll : Dice Rolled -> Fallout -> Result error Fallout
roll dice =
    push (RolledFallout dice) >> Ok


state : Fallout -> State
state (Fallout events) =
    List.foldl handleEvents initialState events


handleEvents : Event -> State -> State
handleEvents event currentState =
    case ( event, currentState ) of
        ( TookDice takenDice, Pending existingDice ) ->
            [ takenDice, existingDice ]
                |> Dice.combine
                |> Pending

        ( RolledFallout falloutDice, Pending _ ) ->
            ExpectingPatientBody falloutDice

        _ ->
            currentState


initialState : State
initialState =
    Pending Dice.empty


push : Event -> Fallout -> Fallout
push event (Fallout events) =
    Fallout (event :: events)
