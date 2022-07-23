module Conflict exposing (Conflict, Error, Side, opponent, play, proponent, start, takeDice)

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
    push side (Played die)



-- HELPERS


push : Side -> Event -> Conflict -> Result error Conflict
push side event (Conflict events) =
    ( side, event )
        :: events
        |> Conflict
        |> Ok



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
