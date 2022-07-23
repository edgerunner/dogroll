module Dice.Type exposing
    ( Background(..)
    , Ceremony(..)
    , DemonicInfluence(..)
    , Fallout(..)
    , Gun(..)
    , Pool(..)
    , Quality(..)
    , Stat(..)
    , Type(..)
    , toDice
    , toString
    )

import Dice exposing (Dice)
import Die.Size exposing (Size(..))
import Pips exposing (Pips)


type Type
    = Stat Stat Pips
    | Trait String Size Pips
    | Relationship String Size Pips
    | Belonging String Quality Gun
    | Initiation
    | DemonicInfluence DemonicInfluence
    | Fallout Fallout Pips
    | Background Background Pool


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


type Pool
    = StatPool
    | TraitPool
    | RelationshipPool


type Background
    = WellRounded
    | StrongHistory
    | ComplicatedHistory
    | StrongCommunity
    | ComplicatedCommunity


toDice : Type -> Dice
toDice type_ =
    case type_ of
        Stat _ pips ->
            toDice_stat pips

        Trait _ size pips ->
            toDice_trait size pips

        Relationship _ size pips ->
            toDice_relationship size pips

        Belonging _ quality gun ->
            [ toDice_quality quality
            , toDice_gun gun
            ]
                |> Dice.combine

        Initiation ->
            toDice_initiation

        DemonicInfluence demonicInfluence ->
            toDice_demonicInfluence demonicInfluence

        Fallout fallout pips ->
            toDice_fallout fallout pips

        Background background pool ->
            toDice_background background pool


toDice_stat : Pips -> Dice
toDice_stat =
    Pips.add Pips.two >> Dice.init D6


toDice_trait : Size -> Pips -> Dice
toDice_trait size =
    Pips.add Pips.one >> Dice.init size


toDice_relationship : Size -> Pips -> Dice
toDice_relationship size =
    Pips.add Pips.one >> Dice.init size


toDice_quality : Quality -> Dice
toDice_quality quality =
    case quality of
        Normal ->
            Dice.init D6 Pips.one

        Excellent ->
            Dice.init D6 Pips.two

        Big ->
            Dice.init D8 Pips.one

        ExcellentPlusBig ->
            Dice.init D8 Pips.two

        Crap ->
            Dice.init D4 Pips.one


toDice_gun : Gun -> Dice
toDice_gun gun =
    case gun of
        Gun ->
            Dice.init D4 Pips.one

        NotGun ->
            Dice.init D4 Pips.zero


toDice_initiation : Dice
toDice_initiation =
    Dice.combine
        [ Dice.init D10 Pips.four
        , Dice.init D6 Pips.four
        ]


toDice_demonicInfluence : DemonicInfluence -> Dice
toDice_demonicInfluence demonicInfluence =
    case demonicInfluence of
        Injustice ->
            Dice.init D10 Pips.one

        DemonicAttacks ->
            Dice.init D10 Pips.two

        Heresy ->
            Dice.init D10 Pips.three

        Sorcery ->
            Dice.init D10 Pips.four

        HateAndMurder ->
            Dice.init D10 Pips.five


toDice_fallout : Fallout -> Pips -> Dice
toDice_fallout fallout =
    case fallout of
        JustTalking ->
            Dice.init D4

        Physical ->
            Dice.init D6

        Fighting ->
            Dice.init D8

        Gunfighting ->
            Dice.init D10

        Ceremony ceremony ceremonies ->
            toDice_ceremony (ceremony :: ceremonies)


toDice_ceremony : List Ceremony -> Pips -> Dice
toDice_ceremony ceremonies =
    ceremonies
        |> List.map
            (\ceremony ->
                case ceremony of
                    AnointingWithSacredEarth ->
                        D8

                    CallingByName ->
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
        |> Die.Size.largest
        |> Dice.init


toDice_background : Background -> Pool -> Dice
toDice_background background pool =
    Dice.combine
        (case ( background, pool ) of
            ( WellRounded, StatPool ) ->
                [ Dice.init D6 (Pips.fromInt 17) ]

            ( WellRounded, TraitPool ) ->
                [ Dice.init D4 Pips.one
                , Dice.init D6 Pips.two
                , Dice.init D8 Pips.four
                ]

            ( WellRounded, RelationshipPool ) ->
                [ Dice.init D6 Pips.four
                , Dice.init D8 Pips.two
                ]

            ( StrongHistory, StatPool ) ->
                [ Dice.init D6 (Pips.fromInt 13) ]

            ( StrongHistory, TraitPool ) ->
                [ Dice.init D6 Pips.three
                , Dice.init D8 Pips.four
                , Dice.init D10 Pips.three
                ]

            ( StrongHistory, RelationshipPool ) ->
                [ Dice.init D4 Pips.one
                , Dice.init D6 Pips.four
                , Dice.init D8 Pips.two
                ]

            ( ComplicatedHistory, StatPool ) ->
                [ Dice.init D6 (Pips.fromInt 15) ]

            ( ComplicatedHistory, TraitPool ) ->
                [ Dice.init D4 Pips.four
                , Dice.init D6 Pips.two
                , Dice.init D10 Pips.two
                ]

            ( ComplicatedHistory, RelationshipPool ) ->
                [ Dice.init D6 Pips.five
                , Dice.init D8 Pips.two
                ]

            ( StrongCommunity, StatPool ) ->
                [ Dice.init D6 (Pips.fromInt 13) ]

            ( StrongCommunity, TraitPool ) ->
                [ Dice.init D4 Pips.one
                , Dice.init D6 Pips.three
                , Dice.init D8 Pips.two
                ]

            ( StrongCommunity, RelationshipPool ) ->
                [ Dice.init D6 Pips.four
                , Dice.init D8 Pips.four
                , Dice.init D10 Pips.three
                ]

            ( ComplicatedCommunity, StatPool ) ->
                [ Dice.init D6 (Pips.fromInt 15) ]

            ( ComplicatedCommunity, TraitPool ) ->
                [ Dice.init D6 (Pips.fromInt 6)
                , Dice.init D8 Pips.two
                ]

            ( ComplicatedCommunity, RelationshipPool ) ->
                [ Dice.init D4 Pips.four
                , Dice.init D6 Pips.two
                , Dice.init D8 Pips.two
                , Dice.init D10 Pips.two
                ]
        )


toString : Type -> String
toString type_ =
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
            case somethingWrong of
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
                                |> List.map toString_ceremony
                                |> String.join ", "
                   )

        Background background _ ->
            case background of
                WellRounded ->
                    "Well-rounded"

                StrongHistory ->
                    "Strong history"

                ComplicatedHistory ->
                    "Complicated history"

                StrongCommunity ->
                    "Strong community"

                ComplicatedCommunity ->
                    "Complicated community"


toString_ceremony : Ceremony -> String
toString_ceremony ceremony =
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
