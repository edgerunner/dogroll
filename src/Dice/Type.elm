module Dice.Type exposing (..)

import Dice exposing (Dice)
import Dice.Pips as Pips exposing (Pips)
import Die.Size exposing (Size(..))


type Type
    = Stat Stat Pips
    | Trait String Size Pips
    | Relationship String Size Pips
    | Belonging String Quality Gun
    | Initiation
    | DemonicInfluence DemonicInfluence
    | Fallout Fallout Pips


type Stat
    = Acuity
    | Heart
    | Body
    | Will


type Gun
    = Gun
    | NotGun


type Quality
    = Normal
    | Excellent
    | Big
    | ExcellentPlusBig
    | Crap


type DemonicInfluence
    = Injustice
    | DemonicAttacks
    | Heresy
    | Sorcery
    | HateAndMurder


type Fallout
    = JustTalking
    | Physical
    | Fighting
    | Gunfighting
    | Ceremony Ceremony (List Ceremony)


type Ceremony
    = AnointingWithSacredEarth
    | CallingByName
    | InvokingTheAncients
    | LayingOnHands
    | MakingTheSignOfTheTree
    | RecitingTheBookOfLife
    | SingingPraise
    | ThreeInAuthority


toDice : Int -> Type -> Dice
toDice seed type_ =
    let
        addExtras base =
            case type_ of
                Belonging _ _ Gun ->
                    Dice.combine [ base, Dice.init (seed - 1) 1 D4 ]

                Initiation ->
                    Dice.combine [ base, Dice.init (seed - 1) 4 D6 ]

                _ ->
                    base
    in
    Dice.init
        seed
        (count type_)
        (size type_)
        |> addExtras


name : Type -> String
name type_ =
    case type_ of
        Stat stat _ ->
            case stat of
                Acuity ->
                    "Acuity"

                Heart ->
                    "Heart"

                Body ->
                    "Body"

                Will ->
                    "Will"

        Trait name_ _ _ ->
            name_

        Relationship name_ _ _ ->
            name_

        Belonging name_ _ _ ->
            name_

        Initiation ->
            "Initiation"

        DemonicInfluence somethingWrong ->
            "Demonic influence: "
                ++ (case somethingWrong of
                        Injustice ->
                            "Injustice"

                        DemonicAttacks ->
                            "Demonic attacks"

                        Heresy ->
                            "Heresy"

                        Sorcery ->
                            "Sorcery"

                        HateAndMurder ->
                            "Hate and murder"
                   )

        Fallout fallout _ ->
            "Fallout: "
                ++ (case fallout of
                        JustTalking ->
                            "Just Talking"

                        Physical ->
                            "Physical"

                        Fighting ->
                            "Fighting"

                        Gunfighting ->
                            "Gunfighting"

                        Ceremony first ceremonies ->
                            (first :: ceremonies)
                                |> List.map
                                    (\ceremony ->
                                        case ceremony of
                                            CallingByName ->
                                                "Calling by name"

                                            AnointingWithSacredEarth ->
                                                "Anointing with sacred earth"

                                            InvokingTheAncients ->
                                                "Invoking the ancients"

                                            LayingOnHands ->
                                                "Laying on hands"

                                            MakingTheSignOfTheTree ->
                                                "Making the sign of the tree"

                                            RecitingTheBookOfLife ->
                                                "Reciting the book of life"

                                            SingingPraise ->
                                                "Singing praise"

                                            ThreeInAuthority ->
                                                "Three in authority"
                                    )
                                |> String.join ", "
                                |> (++) "Ceremony: "
                   )


size : Type -> Size
size type_ =
    case type_ of
        Stat _ _ ->
            D6

        Trait _ size_ _ ->
            size_

        Relationship _ size_ _ ->
            size_

        Belonging _ quality _ ->
            case quality of
                Normal ->
                    D6

                Excellent ->
                    D6

                Big ->
                    D8

                ExcellentPlusBig ->
                    D8

                Crap ->
                    D4

        Initiation ->
            D10

        DemonicInfluence _ ->
            D10

        Fallout fallout _ ->
            case fallout of
                JustTalking ->
                    D4

                Physical ->
                    D6

                Fighting ->
                    D8

                Gunfighting ->
                    D10

                Ceremony first ceremonies ->
                    List.map
                        (\ceremony ->
                            case ceremony of
                                CallingByName ->
                                    D8

                                AnointingWithSacredEarth ->
                                    D4

                                InvokingTheAncients ->
                                    D4

                                LayingOnHands ->
                                    D6

                                MakingTheSignOfTheTree ->
                                    D6

                                RecitingTheBookOfLife ->
                                    D4

                                SingingPraise ->
                                    D6

                                ThreeInAuthority ->
                                    D8
                        )
                        (first :: ceremonies)
                        |> Die.Size.largest


pips : Type -> Pips
pips type_ =
    case type_ of
        Stat _ pips_ ->
            Pips.add Pips.two pips_

        Trait _ _ pips_ ->
            Pips.add Pips.one pips_

        Relationship _ _ pips_ ->
            Pips.add Pips.one pips_

        Belonging _ quality _ ->
            case quality of
                Normal ->
                    Pips.one

                Excellent ->
                    Pips.two

                Big ->
                    Pips.one

                ExcellentPlusBig ->
                    Pips.two

                Crap ->
                    Pips.one

        Initiation ->
            Pips.four

        DemonicInfluence somethingWrong ->
            case somethingWrong of
                Injustice ->
                    Pips.one

                DemonicAttacks ->
                    Pips.two

                Heresy ->
                    Pips.three

                Sorcery ->
                    Pips.four

                HateAndMurder ->
                    Pips.five

        Fallout _ pips_ ->
            pips_


count : Type -> Int
count =
    pips >> Pips.toInt
