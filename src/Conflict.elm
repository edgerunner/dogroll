module Conflict exposing (Conflict, Error(..), Player, Raise(..), See(..), Side(..), State, give, initialState, keptDie, opponent, otherSide, play, player, proponent, raise, see, start, state, takeDice, takeFallout)

import Dice
import Die
import Die.Size exposing (Size)
import Pips exposing (Pips)


type alias Die =
    Die.Die Die.Rolled


type alias Dice =
    Dice.Dice Die.Rolled


type Conflict
    = Conflict (List ( Side, Event ))


type Event
    = TookDice Dice
    | Played Die
    | Raised
    | Seen
    | TookFallout Size
    | Gave



-- ACTIONS


start : Conflict
start =
    Conflict []


takeDice : Dice -> Side -> Conflict -> Result error Conflict
takeDice dice side =
    push side (TookDice dice)


play : Die -> Side -> Conflict -> Result Error Conflict
play die side =
    check
        (player side
            >> .pool
            >> Dice.has die
            >> toError DieNotInPool
        )
        >> Result.andThen (checkPlayerTurn side)
        >> Result.andThen (check (mustTakeFallout >> not >> toError MustTakeFallout))
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


see : Side -> Conflict -> Result Error Conflict
see side =
    check (.raise >> canSee >> toError NotEnoughToSee)
        >> Result.andThen (checkPlayerTurn side)
        >> Result.andThen (push side Seen)


takeFallout : Size -> Side -> Conflict -> Result Error Conflict
takeFallout size side =
    check
        (mustTakeFallout >> toError NoPendingFallout)
        >> Result.andThen (checkPlayerTurn side)
        >> Result.andThen (push side (TookFallout size))


give : Side -> Conflict -> Result Error Conflict
give side =
    checkPlayerTurn side
        >> Result.andThen (push side Gave)



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


canSee : Raise -> Bool
canSee raise_ =
    case raise_ of
        RaisedWith raise1 raise2 see_ ->
            let
                raiseTotal =
                    total [ raise1, raise2 ]

                total =
                    List.foldl Dice.add Dice.empty
                        >> Dice.total
            in
            case see_ of
                LoseTheStakes ->
                    False

                ReverseTheBlow see1 ->
                    raiseTotal <= total [ see1 ]

                BlockOrDodge see1 see2 ->
                    raiseTotal <= total [ see1, see2 ]

                TakeTheBlow see1 see2 see3 seeMore ->
                    raiseTotal <= total (see1 :: see2 :: see3 :: seeMore)

        _ ->
            False


mustTakeFallout : State -> Bool
mustTakeFallout current =
    case current.raise of
        PendingFallout _ ->
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
    = DieNotInPool
    | RaiseWithTwoDice
    | NotYourTurn
    | NotEnoughToSee
    | MustTakeFallout
    | NoPendingFallout



-- STATE


type alias State =
    { proponent : Player
    , opponent : Player
    , raise : Raise
    , go : Side
    }


type alias Player =
    { pool : Dice
    , fallout : Dice.Dice Die.Held
    , bestDie : Maybe Die
    }


type Raise
    = PendingTwoDice
    | PendingOneDie Die
    | ReadyToRaise Die Die
    | RaisedWith Die Die See
    | PendingFallout Pips
    | GivenUp (Maybe Die)


type See
    = LoseTheStakes
    | ReverseTheBlow Die
    | BlockOrDodge Die Die
    | TakeTheBlow Die Die Die (List Die)


state : Conflict -> State
state (Conflict events) =
    List.foldr handleEvent initialState events


handleEvent : ( Side, Event ) -> State -> State
handleEvent sideEvent current =
    case sideEvent of
        ( side, TookDice dice ) ->
            updatePlayer
                (\currentPlayer ->
                    { currentPlayer
                        | pool = Dice.combine [ dice, currentPlayer.pool ]
                        , bestDie =
                            dice
                                |> (currentPlayer.bestDie
                                        |> Maybe.map Dice.add
                                        |> Maybe.withDefault identity
                                   )
                                |> (Dice.toList >> List.head)
                    }
                )
                side
                current

        ( side, Played die ) ->
            { current | raise = raiseWith die current.raise }
                |> updatePlayer
                    (\currentPlayer ->
                        { currentPlayer | pool = Dice.drop die currentPlayer.pool }
                    )
                    side

        ( side, Raised ) ->
            { current | raise = finalizeRaise current.raise, go = otherSide side }

        ( _, Seen ) ->
            { current | raise = finalizeSee current.raise }

        ( side, TookFallout size ) ->
            { current | raise = PendingTwoDice }
                |> updatePlayer
                    (\currentPlayer ->
                        { currentPlayer
                            | fallout =
                                Dice.combine
                                    [ Dice.init size <| pendingFallout current
                                    , currentPlayer.fallout
                                    ]
                        }
                    )
                    side

        ( side, Gave ) ->
            { current | raise = finalizeGive (player side current |> .bestDie) current.raise }


otherSide : Side -> Side
otherSide side =
    case side of
        Proponent ->
            Opponent

        Opponent ->
            Proponent


finalizeRaise : Raise -> Raise
finalizeRaise raise_ =
    case raise_ of
        ReadyToRaise die1 die2 ->
            RaisedWith die1 die2 LoseTheStakes

        _ ->
            raise_


finalizeSee : Raise -> Raise
finalizeSee raise_ =
    case raise_ of
        RaisedWith _ _ see_ ->
            case see_ of
                ReverseTheBlow see1 ->
                    PendingOneDie see1

                BlockOrDodge _ _ ->
                    PendingTwoDice

                TakeTheBlow _ _ _ extraDice ->
                    PendingFallout (extraDice |> Pips.fromList |> Pips.add Pips.three)

                LoseTheStakes ->
                    raise_

        _ ->
            PendingTwoDice


finalizeGive : Maybe Die -> Raise -> Raise
finalizeGive bestDie_ raise_ =
    case raise_ of
        RaisedWith _ _ _ ->
            GivenUp Nothing

        _ ->
            GivenUp bestDie_


raiseWith : Die -> Raise -> Raise
raiseWith die raise_ =
    case raise_ of
        PendingTwoDice ->
            PendingOneDie die

        PendingOneDie firstDie ->
            ReadyToRaise firstDie die

        RaisedWith firstDie secondDie see_ ->
            seeWith die see_
                |> RaisedWith firstDie secondDie

        _ ->
            raise_


seeWith : Die -> See -> See
seeWith die see_ =
    case see_ of
        LoseTheStakes ->
            ReverseTheBlow die

        ReverseTheBlow firstDie ->
            BlockOrDodge firstDie die

        BlockOrDodge firstDie secondDie ->
            TakeTheBlow firstDie secondDie die []

        TakeTheBlow firstDie secondDie thirdDie extraDice ->
            TakeTheBlow firstDie secondDie thirdDie (die :: extraDice)


initialState : State
initialState =
    { proponent = { pool = Dice.empty, fallout = Dice.empty, bestDie = Nothing }
    , opponent = { pool = Dice.empty, fallout = Dice.empty, bestDie = Nothing }
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


pendingFallout : State -> Pips
pendingFallout current =
    case current.raise of
        PendingFallout size ->
            size

        _ ->
            Pips.zero


updatePlayer : (Player -> Player) -> Side -> State -> State
updatePlayer update side current =
    case side of
        Proponent ->
            { current | proponent = update current.proponent }

        Opponent ->
            { current | opponent = update current.opponent }


keptDie : Conflict -> Maybe Die
keptDie =
    state
        >> (\current ->
                case current.raise of
                    GivenUp keptDie_ ->
                        keptDie_

                    _ ->
                        Nothing
           )
