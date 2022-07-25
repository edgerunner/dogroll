module Conflict exposing (Conflict, Error, Side, State, opponent, play, proponent, raise, start, state, takeDice)

import Dice exposing (Dice)
import Die exposing (Die)


type Conflict
    = Conflict (List ( Side, Event ))


type Event
    = TookDice Dice
    | Played Die
    | Raised



-- ACTIONS


start : Result error Conflict
start =
    Ok <| Conflict []


takeDice : Side -> Dice -> Conflict -> Result Error Conflict
takeDice side dice =
    if Dice.allRolled dice then
        push side (TookDice dice)

    else
        Err DiceNotRolled |> always


play : Side -> Die -> Conflict -> Result Error Conflict
play side die =
    check
        (player side
            >> .pool
            >> Dice.has die
            >> toError DieNotInPool
        )
        >> Result.andThen (checkPlayerTurn side)
        >> Result.andThen
            (check
                (.raise >> readyToRaise >> not >> toError RaiseWithTwoDice)
            )
        >> Result.andThen (push side (Played die))


raise : Side -> Conflict -> Result Error Conflict
raise side =
    check (.raise >> readyToRaise >> toError RaiseWithTwoDice)
        >> Result.andThen (checkPlayerTurn side)
        >> Result.andThen (push side Raised)



-- HELPERS


push : Side -> Event -> Conflict -> Result error Conflict
push side event (Conflict events) =
    ( side, event )
        :: events
        |> Conflict
        |> Ok


check : (State -> Result Error any) -> Conflict -> Result Error Conflict
check predicate conflict =
    conflict |> state |> predicate |> Result.map (always conflict)


toError : Error -> Bool -> Result Error ()
toError error bool =
    if bool then
        Ok ()

    else
        Err error



-- CHECKS


checkPlayerTurn : Side -> Conflict -> Result Error Conflict
checkPlayerTurn side =
    check (.go >> (==) side >> toError NotYourTurn)


readyToRaise : Raise -> Bool
readyToRaise raise_ =
    case raise_ of
        ReadyToRaise _ _ ->
            True

        _ ->
            False



-- SIDE TYPE


type Side
    = Proponent
    | Opponent


proponent : Side
proponent =
    Proponent


opponent : Side
opponent =
    Opponent



-- ERROR TYPE


type Error
    = DiceNotRolled
    | DieNotInPool
    | RaiseWithTwoDice
    | NotYourTurn



-- STATE


type alias State =
    { proponent : Player
    , opponent : Player
    , raise : Raise
    , go : Side
    }


type alias Player =
    { pool : Dice }


type Raise
    = PendingTwoDice
    | PendingOneDie Die
    | ReadyToRaise Die Die
    | RaisedWith Die Die


state : Conflict -> State
state (Conflict events) =
    List.foldr handleEvent initialState events


handleEvent : ( Side, Event ) -> State -> State
handleEvent sideEvent current =
    case sideEvent of
        ( Proponent, TookDice dice ) ->
            { current | proponent = { pool = dice } }

        ( Opponent, TookDice dice ) ->
            { current | opponent = { pool = dice } }

        ( Proponent, Played die ) ->
            { current
                | proponent = { pool = Dice.drop die current.proponent.pool }
                , raise = raiseWith die current.raise
            }

        ( Opponent, Played die ) ->
            { current | opponent = { pool = Dice.drop die current.opponent.pool } }

        ( _, Raised ) ->
            { current | raise = finalizeRaise current.raise }


finalizeRaise : Raise -> Raise
finalizeRaise raise_ =
    case raise_ of
        ReadyToRaise die1 die2 ->
            RaisedWith die1 die2

        _ ->
            raise_


raiseWith : Die -> Raise -> Raise
raiseWith die raise_ =
    case raise_ of
        PendingTwoDice ->
            PendingOneDie die

        PendingOneDie firstDie ->
            ReadyToRaise firstDie die

        _ ->
            raise_


initialState : State
initialState =
    { proponent = { pool = Dice.empty }
    , opponent = { pool = Dice.empty }
    , raise = PendingTwoDice
    , go = proponent
    }


player : Side -> State -> Player
player side_ =
    case side_ of
        Proponent ->
            .proponent

        Opponent ->
            .opponent
