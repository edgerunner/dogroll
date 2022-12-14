module Die.Size exposing (Size(..), Sizes, all, count, get, largest, toInt, toString)


type Size
    = D4
    | D6
    | D8
    | D10


toString : Size -> String
toString size =
    case size of
        D4 ->
            "d4"

        D6 ->
            "d6"

        D8 ->
            "d8"

        D10 ->
            "d10"


toInt : Size -> Int
toInt size =
    case size of
        D4 ->
            4

        D6 ->
            6

        D8 ->
            8

        D10 ->
            10


all : List Size
all =
    [ D10, D8, D6, D4 ]


type alias Sizes a =
    { d4 : a
    , d6 : a
    , d8 : a
    , d10 : a
    }


count : List Size -> Sizes Int
count =
    List.foldl
        (\size sizes ->
            case size of
                D4 ->
                    { sizes | d4 = sizes.d4 + 1 }

                D6 ->
                    { sizes | d6 = sizes.d6 + 1 }

                D8 ->
                    { sizes | d8 = sizes.d8 + 1 }

                D10 ->
                    { sizes | d10 = sizes.d10 + 1 }
        )
        { d4 = 0, d6 = 0, d8 = 0, d10 = 0 }


largest : List Size -> Size
largest sizes =
    case sizes of
        [] ->
            D4

        [ size ] ->
            size

        s1 :: s2 :: rest ->
            larger s1 s2 :: rest |> largest


larger : Size -> Size -> Size
larger s1 s2 =
    case ( s1, s2 ) of
        ( D10, _ ) ->
            D10

        ( _, D10 ) ->
            D10

        ( D8, _ ) ->
            D8

        ( _, D8 ) ->
            D8

        ( D6, _ ) ->
            D6

        ( _, D6 ) ->
            D6

        _ ->
            D4


get : Size -> Sizes a -> a
get size =
    case size of
        D4 ->
            .d4

        D6 ->
            .d6

        D8 ->
            .d8

        D10 ->
            .d10
