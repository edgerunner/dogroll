module Conflict exposing (Conflict, Error, Side, State, opponent, play, proponent, start, state, takeDice)

import Dice exposing (Dice)
import Die exposing (Die)


type Conflict
    = Conflict (List ( Side, Event ))


type Event
    = TookDice Dice
    | Played Die



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
        (\current ->
            case side of
                Proponent ->
                    if Dice.has die current.proponent.pool then
                        Ok ()

                    else
                        Err DieNotInPool

                Opponent ->
                    if Dice.has die current.opponent.pool then
                        Ok ()

                    else
                        Err DieNotInPool
        )
        >> Result.andThen (push side (Played die))



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



-- STATE


type alias State =
    { proponent : Player
    , opponent : Player
    }


type alias Player =
    { pool : Dice }


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
            { current | proponent = { pool = Dice.drop die current.proponent.pool } }

        ( Opponent, Played die ) ->
            { current | opponent = { pool = Dice.drop die current.opponent.pool } }


initialState : State
initialState =
    { proponent = { pool = Dice.empty }
    , opponent = { pool = Dice.empty }
    }
