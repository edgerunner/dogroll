module Fallout exposing
    ( Fallout, init
    , takeHealerAcuityDice, takePatientBodyDice, takeDemonicInfluenceDice, endConflict
    , roll, rollPatientBody, startConflict
    , test_roll, test_rollPatientBody, test_startConflict
    , State(..), ConflictDice, Outcome(..), state
    , Experience(..), experience
    , Error(..)
    )

{-| The fallout module encapsulates the core logic behind resolving the fallout from
a regular conflict. It records and reduces events to calculate its state.


## Fallout and events

@docs Fallout, init


## Actions

These have the signature `Fallout -> Result Error Fallout`.
If they are successful, they return the new fallout with the new event.
If they fail, they return the error. Failing actions don't create events.

@docs takeHealerAcuityDice, takePatientBodyDice, takeDemonicInfluenceDice, endConflict


### Generator actions

These actions return a generator that produces the next fallout instance
with a new event when seeded. `Fallout -> Result Error (Generator Fallout)`.

@docs roll, rollPatientBody, startConflict


#### Testing generators

Generators are impossible to test directly. These functions
are stand-in event creators that replace the generator actions
for testing purposes.

@docs test_roll, test_rollPatientBody, test_startConflict


## State

State represents the resolution of the fallout.

@docs State, ConflictDice, Outcome, state


### Experience

Experience is a parallel state, representing if the fallout subject
can choose from the _experience fallout list_

@docs Experience, experience

-}

import Conflict exposing (Conflict, Raise(..), Side(..))
import Dice exposing (Dice)
import Die exposing (Die, Held, Rolled)
import Die.Size exposing (Size(..))
import Pips exposing (Pips)
import Random exposing (Generator)


type Fallout
    = Fallout (List Event)


type Event
    = TookDice (Dice Held)
    | RolledFallout (Dice Rolled)
    | RolledPatientBody (Dice Rolled)
    | TookPatientBodyDice (Dice Held)
    | TookHealerAcuityDice (Dice Held)
    | TookDemonicInfluenceDice (Dice Held)
    | StartedConflict Conflict
    | EndedConflict Conflict


type State
    = Pending (Dice Held)
    | ExpectingPatientBody (Dice Rolled)
    | ExpectingDice ConflictDice
    | InConflict Conflict
    | Concluded Outcome


type alias ConflictDice =
    { fallout : Dice Held
    , patientBody : Maybe (Dice Held)
    , healerAcuity : Maybe (Dice Held)
    , demonicInfluence : Maybe (Dice Held)
    }


type Outcome
    = ShortTerm
    | LongTerm
    | DoubleLongTerm
    | Dying


type Error
    = FalloutDiceCantBeEmpty
    | CannotRollFalloutMoreThanOnce
    | CannotTakePatientBodyDiceAfterRolling
    | NotExpectingDice
    | UnableToStartConflict
    | NotInConflict
    | WrongConflict
    | ConflictNotOver


init : Dice Held -> Result Error Fallout
init dice =
    if dice /= Dice.empty then
        Ok <| Fallout [ TookDice dice ]

    else
        Err FalloutDiceCantBeEmpty


roll : Fallout -> Result Error (Generator Fallout)
roll fallout =
    case state fallout of
        Pending pendingDice ->
            Dice.generator pendingDice
                |> Random.map (RolledFallout >> push >> (|>) fallout)
                |> Ok

        _ ->
            Err CannotRollFalloutMoreThanOnce


test_roll : Dice Rolled -> Fallout -> Fallout
test_roll dice =
    push (RolledFallout dice)


rollPatientBody : Pips -> Fallout -> Result Error (Generator Fallout)
rollPatientBody pips fallout =
    Dice.init D6 pips
        |> Dice.generator
        |> Random.map (RolledPatientBody >> push >> (|>) fallout)
        |> Ok


test_rollPatientBody : Dice Rolled -> Fallout -> Fallout
test_rollPatientBody dice =
    push (RolledPatientBody dice)


takePatientBodyDice : Pips -> Fallout -> Result Error Fallout
takePatientBodyDice pips =
    checkExpectingDice
        (.patientBody
            >> (==) Nothing
            >> toError CannotTakePatientBodyDiceAfterRolling
        )
        >> Result.map (push (TookPatientBodyDice <| Dice.init D6 pips))


takeHealerAcuityDice : Pips -> Fallout -> Result Error Fallout
takeHealerAcuityDice pips =
    checkExpectingDice Ok
        >> Result.map (push (TookHealerAcuityDice <| Dice.init D6 pips))


takeDemonicInfluenceDice : Pips -> Fallout -> Result Error Fallout
takeDemonicInfluenceDice pips =
    checkExpectingDice Ok
        >> Result.map (push (TookDemonicInfluenceDice <| Dice.init D10 pips))


startConflict : Fallout -> Result Error (Generator Fallout)
startConflict fallout =
    state fallout
        |> (\current ->
                case current of
                    ExpectingDice dice ->
                        conflictGenerator dice
                            |> Result.map
                                (Random.map (StartedConflict >> push >> (|>) fallout))

                    _ ->
                        Err UnableToStartConflict
           )


test_startConflict : Dice Rolled -> Dice Rolled -> Fallout -> Fallout
test_startConflict proponentDice opponentDice =
    Conflict.start
        |> Conflict.takeDice proponentDice Conflict.proponent
        |> Result.andThen (Conflict.takeDice opponentDice Conflict.opponent)
        |> Result.withDefault Conflict.start
        |> StartedConflict
        |> push


endConflict : Conflict -> Fallout -> Result Error Fallout
endConflict endedConflict =
    check
        (\current ->
            case current of
                InConflict ongoingConflict ->
                    if Conflict.match ongoingConflict endedConflict then
                        endedConflict
                            |> Conflict.state
                            |> (\conflictState ->
                                    case conflictState.raise of
                                        GivenUp _ ->
                                            Ok ()

                                        _ ->
                                            Err ConflictNotOver
                               )

                    else
                        Err WrongConflict

                _ ->
                    Err NotInConflict
        )
        >> Result.map (push (EndedConflict endedConflict))



-- STATE


state : Fallout -> State
state (Fallout events) =
    List.foldr handleEvents initialState events


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
                Concluded ShortTerm

            else if falloutSum <= 11 then
                Concluded LongTerm

            else if falloutSum <= 15 then
                ExpectingPatientBody falloutDice

            else if falloutSum <= 19 then
                ExpectingDice
                    { fallout =
                        falloutDice
                            |> Dice.sizes
                            |> List.foldl (Die.init >> Dice.add) Dice.empty
                    , patientBody = Nothing
                    , healerAcuity = Nothing
                    , demonicInfluence = Nothing
                    }

            else
                Concluded Dying

        ( RolledPatientBody patientBodyDice, ExpectingPatientBody falloutDice ) ->
            if
                (patientBodyDice
                    |> Dice.best 3
                    |> Dice.total
                )
                    >= (falloutDice
                            |> Dice.best 2
                            |> Dice.total
                       )
            then
                Concluded DoubleLongTerm

            else
                ExpectingDice
                    { fallout =
                        falloutDice
                            |> Dice.sizes
                            |> List.foldl (Die.init >> Dice.add) Dice.empty
                    , patientBody =
                        patientBodyDice
                            |> Dice.sizes
                            |> List.foldl (Die.init >> Dice.add) Dice.empty
                            |> Just
                    , healerAcuity = Nothing
                    , demonicInfluence = Nothing
                    }

        ( TookPatientBodyDice patientBodyDice, ExpectingDice dice ) ->
            ExpectingDice { dice | patientBody = Just patientBodyDice }

        ( TookHealerAcuityDice healerAcuityDice, ExpectingDice dice ) ->
            ExpectingDice { dice | healerAcuity = Just healerAcuityDice }

        ( TookDemonicInfluenceDice demonicInfluenceDice, ExpectingDice dice ) ->
            ExpectingDice { dice | demonicInfluence = Just demonicInfluenceDice }

        ( StartedConflict conflict, _ ) ->
            InConflict conflict

        ( EndedConflict endedConflict, InConflict _ ) ->
            case Conflict.state endedConflict |> .go of
                Proponent ->
                    Concluded Dying

                Opponent ->
                    Concluded DoubleLongTerm

        _ ->
            currentState


initialState : State
initialState =
    Pending Dice.empty



-- EXPERIENCE


type Experience
    = Indeterminate
    | Experience (Die Rolled)
    | NoExperience


experience : Fallout -> Experience
experience (Fallout events) =
    case events of
        [] ->
            Indeterminate

        (RolledFallout dice) :: _ ->
            dice
                |> Dice.toList
                |> List.filter (Die.face >> (==) 1)
                |> List.head
                |> Maybe.map Experience
                |> Maybe.withDefault NoExperience

        _ :: rest ->
            experience (Fallout rest)



-- HELPERS


push : Event -> Fallout -> Fallout
push event (Fallout events) =
    Fallout (event :: events)


check : (State -> Result Error any) -> Fallout -> Result Error Fallout
check predicate fallout =
    fallout |> state |> predicate |> Result.map (always fallout)


toError : Error -> Bool -> Result Error ()
toError error bool =
    if bool then
        Ok ()

    else
        Err error



-- CHECKS


checkExpectingDice : (ConflictDice -> Result Error any) -> Fallout -> Result Error Fallout
checkExpectingDice predicate =
    check
        (\current ->
            case current of
                ExpectingDice dice ->
                    predicate dice

                _ ->
                    Err NotExpectingDice
        )



-- GENERATORS


conflictGenerator : ConflictDice -> Result Error (Generator Conflict)
conflictGenerator dice =
    let
        rolledToConflict fallout body acuity demonic =
            Ok Conflict.start
                |> Result.andThen (Conflict.takeDice body Conflict.proponent)
                |> Result.andThen (Conflict.takeDice acuity Conflict.proponent)
                |> Result.andThen (Conflict.takeDice demonic Conflict.opponent)
                |> Result.andThen (Conflict.takeDice fallout Conflict.opponent)
                |> Result.withDefault Conflict.start

        heldToGenerator fallout body acuity demonic =
            Random.map4
                rolledToConflict
                (Dice.generator fallout)
                (Dice.generator body)
                (Dice.generator acuity)
                (Dice.generator demonic)
    in
    Maybe.map3
        (heldToGenerator dice.fallout)
        dice.patientBody
        dice.healerAcuity
        dice.demonicInfluence
        |> Result.fromMaybe UnableToStartConflict
