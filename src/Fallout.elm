module Fallout exposing (Fallout, Outcome(..), State(..), init, roll, rollPatientBody, state, takeDemonicInfluenceDice, takeDice, takeHealerAcuityDice, takePatientBodyDice)

import Conflict exposing (Conflict)
import Dice exposing (Dice)
import Die exposing (Held, Rolled)


type Fallout
    = Fallout (List Event)


type Event
    = TookDice (Dice Held)
    | RolledFallout (Dice Rolled)
    | RolledPatientBody (Dice Rolled)
    | TookPatientBodyDice (Dice Held)
    | TookHealerAcuityDice (Dice Held)
    | TookDemonicInfluenceDice (Dice Held)
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
    | CannotTakePatientBodyDiceAfterRolling
    | NotExpectingDice


init : Fallout
init =
    Fallout []


takeDice : Dice Held -> Fallout -> Result Error Fallout
takeDice dice =
    check
        (\current ->
            case current of
                Pending _ ->
                    Ok ()

                _ ->
                    Err CanNotTakeFalloutDiceAfterRolling
        )
        >> Result.map (push (TookDice dice))


roll : Dice Rolled -> Fallout -> Result Error Fallout
roll rolledDice =
    check
        (\current ->
            case current of
                Pending pendingDice ->
                    (rolledDice
                        |> Dice.sizes
                        |> List.foldl (Die.init >> Dice.add) Dice.empty
                    )
                        == pendingDice
                        |> toError (MustRollTheFalloutDice pendingDice)

                _ ->
                    Err CannotRollFalloutMoreThanOnce
        )
        >> Result.map (push (RolledFallout rolledDice))


rollPatientBody : Dice Rolled -> Fallout -> Result Error Fallout
rollPatientBody dice fallout =
    fallout |> push (RolledPatientBody dice) |> Ok


takePatientBodyDice : Dice Held -> Fallout -> Result Error Fallout
takePatientBodyDice patientBodyDice =
    check
        (\current ->
            case current of
                ExpectingDice dice ->
                    (dice.patientBody == Nothing)
                        |> toError CannotTakePatientBodyDiceAfterRolling

                _ ->
                    Err NotExpectingDice
        )
        >> Result.map (push (TookPatientBodyDice patientBodyDice))


takeHealerAcuityDice : Dice Held -> Fallout -> Result Error Fallout
takeHealerAcuityDice healerAcuityDice =
    check
        (\current ->
            case current of
                ExpectingDice _ ->
                    Ok ()

                _ ->
                    Err NotExpectingDice
        )
        >> Result.map (push (TookHealerAcuityDice healerAcuityDice))


takeDemonicInfluenceDice : Dice Held -> Fallout -> Result Error Fallout
takeDemonicInfluenceDice demonicInfluenceDice =
    check
        (\current ->
            case current of
                ExpectingDice _ ->
                    Ok ()

                _ ->
                    Err NotExpectingDice
        )
        >> Result.map (push (TookDemonicInfluenceDice demonicInfluenceDice))


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
                Concluded False ShortTerm

            else if falloutSum <= 11 then
                Concluded False LongTerm

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
                Concluded False Dying

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
                Concluded False DoubleLongTerm

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

        _ ->
            currentState


initialState : State
initialState =
    Pending Dice.empty



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
