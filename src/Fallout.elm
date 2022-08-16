module Fallout exposing (ConflictDice, Fallout, Outcome(..), State(..), init, roll, rollPatientBody, startConflict, state, takeDemonicInfluenceDice, takeDice, takeHealerAcuityDice, takePatientBodyDice)

import Conflict exposing (Conflict)
import Dice exposing (Dice)
import Die exposing (Held, Rolled)
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
    | Concluded Bool Outcome


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
    = CanNotTakeFalloutDiceAfterRolling
    | CannotRollFalloutMoreThanOnce
    | MustRollTheFalloutDice (Dice Held)
    | CannotTakePatientBodyDiceAfterRolling
    | NotExpectingDice
    | UnableToStartConflict


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
    checkExpectingDice
        (.patientBody
            >> (==) Nothing
            >> toError CannotTakePatientBodyDiceAfterRolling
        )
        >> Result.map (push (TookPatientBodyDice patientBodyDice))


takeHealerAcuityDice : Dice Held -> Fallout -> Result Error Fallout
takeHealerAcuityDice healerAcuityDice =
    checkExpectingDice Ok
        >> Result.map (push (TookHealerAcuityDice healerAcuityDice))


takeDemonicInfluenceDice : Dice Held -> Fallout -> Result Error Fallout
takeDemonicInfluenceDice demonicInfluenceDice =
    checkExpectingDice Ok
        >> Result.map (push (TookDemonicInfluenceDice demonicInfluenceDice))


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
                (Dice.generator body)
                (Dice.generator acuity)
                (Dice.generator demonic)
                (Dice.generator fallout)
    in
    Maybe.map3
        (heldToGenerator dice.fallout)
        dice.patientBody
        dice.healerAcuity
        dice.demonicInfluence
        |> Result.fromMaybe UnableToStartConflict
