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


type Error
    = CanNotTakeFalloutDiceAfterRolling
    | CannotRollFalloutMoreThanOnce
    | MustRollTheFalloutDice (Dice Held)


init : Fallout
init =
    Fallout []


takeDice : Dice Held -> Fallout -> Result Error Fallout
takeDice dice fallout =
    state fallout
        |> (\current ->
                case current of
                    Pending _ ->
                        fallout |> push (TookDice dice) |> Ok

                    _ ->
                        Err CanNotTakeFalloutDiceAfterRolling
           )


roll : Dice Rolled -> Fallout -> Result Error Fallout
roll rolledDice fallout =
    state fallout
        |> (\current ->
                case current of
                    Pending pendingDice ->
                        if
                            (rolledDice
                                |> Dice.sizes
                                |> List.foldl (Die.init >> Dice.add) Dice.empty
                            )
                                == pendingDice
                        then
                            fallout |> push (RolledFallout rolledDice) |> Ok

                        else
                            Err <| MustRollTheFalloutDice pendingDice

                    _ ->
                        Err CannotRollFalloutMoreThanOnce
           )


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
            let
                falloutSum =
                    falloutDice |> Dice.best 2 |> Dice.total
            in
            if falloutSum <= 7 then
                Concluded False ShortTerm

            else
                ExpectingPatientBody falloutDice

        _ ->
            currentState


initialState : State
initialState =
    Pending Dice.empty


push : Event -> Fallout -> Fallout
push event (Fallout events) =
    Fallout (event :: events)
