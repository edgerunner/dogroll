module Die exposing (Die, d4, d6, d8, d10, sides)

type Die
    = Die Sides


type Sides
    = D4
    | D6
    | D8
    | D10

d4 : Die
d4 =
    Die D4

d6 : Die
d6 =
    Die D6

d8 : Die
d8 =
    Die D8

d10 : Die
d10 =
    Die D10

sides : Die -> Int
sides (Die d) =
    case d of
        D4 -> 4
        D6 -> 6
        D8 -> 8
        D10 -> 10