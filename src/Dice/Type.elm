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
import Dice.Pips as Pips exposing (Pips)
import Die.Size exposing (Size(..))
import Random exposing (Seed)


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


toDice : Seed -> Type -> Dice
toDice seed type_ =
    case type_ of
        Stat _ pips ->
            toDice_stat seed pips

        Trait _ size pips ->
            toDice_trait seed size pips

        Relationship _ size pips ->
            toDice_relationship seed size pips

        Belonging _ quality gun ->
            [ toDice_quality seed quality
            , toDice_gun seed gun
            ]
                |> Dice.combine

        Initiation ->
            toDice_initiation seed

        DemonicInfluence demonicInfluence ->
            toDice_demonicInfluence seed demonicInfluence

        Fallout fallout pips ->
            toDice_fallout seed fallout pips

        Background background pool ->
            toDice_background seed background pool


toDice_stat : Seed -> Pips -> Dice
toDice_stat seed pips =
    Dice.init seed (Pips.add Pips.two pips |> Pips.toInt) D6


toDice_trait : Seed -> Size -> Pips -> Dice
toDice_trait seed size pips =
    Dice.init seed (Pips.add Pips.one pips |> Pips.toInt) size


toDice_relationship : Seed -> Size -> Pips -> Dice
toDice_relationship seed size pips =
    Dice.init seed (Pips.add Pips.one pips |> Pips.toInt) size


toDice_quality : Seed -> Quality -> Dice
toDice_quality seed quality =
    case quality of
        Normal ->
            Dice.init seed 1 D6

        Excellent ->
            Dice.init seed 2 D6

        Big ->
            Dice.init seed 1 D8

        ExcellentPlusBig ->
            Dice.init seed 2 D8

        Crap ->
            Dice.init seed 1 D4


toDice_gun : Seed -> Gun -> Dice
toDice_gun seed gun =
    case gun of
        Gun ->
            Dice.init seed 1 D4

        NotGun ->
            Dice.init seed 0 D4


toDice_initiation : Seed -> Dice
toDice_initiation seed =
    Dice.combine
        [ Dice.init seed 4 D10
        , Dice.init seed 4 D6
        ]


toDice_demonicInfluence : Seed -> DemonicInfluence -> Dice
toDice_demonicInfluence seed demonicInfluence =
    case demonicInfluence of
        Injustice ->
            Dice.init seed 1 D10

        DemonicAttacks ->
            Dice.init seed 2 D10

        Heresy ->
            Dice.init seed 3 D10

        Sorcery ->
            Dice.init seed 4 D10

        HateAndMurder ->
            Dice.init seed 5 D10


toDice_fallout : Seed -> Fallout -> Pips -> Dice
toDice_fallout seed fallout pips =
    case fallout of
        JustTalking ->
            Dice.init seed (Pips.toInt pips) D4

        Physical ->
            Dice.init seed (Pips.toInt pips) D6

        Fighting ->
            Dice.init seed (Pips.toInt pips) D8

        Gunfighting ->
            Dice.init seed (Pips.toInt pips) D10

        Ceremony ceremony ceremonies ->
            toDice_ceremony seed (ceremony :: ceremonies) pips


toDice_ceremony : Seed -> List Ceremony -> Pips -> Dice
toDice_ceremony seed ceremonies pips =
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
        |> Dice.init seed (Pips.toInt pips)


toDice_background : Seed -> Background -> Pool -> Dice
toDice_background seed background pool =
    Dice.combine
        (case ( background, pool ) of
            ( WellRounded, StatPool ) ->
                [ Dice.init seed 17 D6 ]

            ( WellRounded, TraitPool ) ->
                [ Dice.init seed 1 D4
                , Dice.init seed 4 D6
                , Dice.init seed 2 D8
                ]

            ( WellRounded, RelationshipPool ) ->
                [ Dice.init seed 4 D6
                , Dice.init seed 2 D8
                ]

            ( StrongHistory, StatPool ) ->
                [ Dice.init seed 13 D6 ]

            ( StrongHistory, TraitPool ) ->
                [ Dice.init seed 3 D6
                , Dice.init seed 4 D8
                , Dice.init seed 3 D10
                ]

            ( StrongHistory, RelationshipPool ) ->
                [ Dice.init seed 1 D4
                , Dice.init seed 3 D6
                , Dice.init seed 2 D8
                ]

            ( ComplicatedHistory, StatPool ) ->
                [ Dice.init seed 15 D6 ]

            ( ComplicatedHistory, TraitPool ) ->
                [ Dice.init seed 4 D4
                , Dice.init seed 2 D6
                , Dice.init seed 2 D10
                ]

            ( ComplicatedHistory, RelationshipPool ) ->
                [ Dice.init seed 5 D6
                , Dice.init seed 2 D8
                ]

            ( StrongCommunity, StatPool ) ->
                [ Dice.init seed 13 D6 ]

            ( StrongCommunity, TraitPool ) ->
                [ Dice.init seed 1 D4
                , Dice.init seed 3 D6
                , Dice.init seed 2 D8
                ]

            ( StrongCommunity, RelationshipPool ) ->
                [ Dice.init seed 4 D6
                , Dice.init seed 4 D8
                , Dice.init seed 3 D10
                ]

            ( ComplicatedCommunity, StatPool ) ->
                [ Dice.init seed 15 D6 ]

            ( ComplicatedCommunity, TraitPool ) ->
                [ Dice.init seed 6 D6
                , Dice.init seed 2 D8
                ]

            ( ComplicatedCommunity, RelationshipPool ) ->
                [ Dice.init seed 4 D4
                , Dice.init seed 2 D6
                , Dice.init seed 2 D8
                , Dice.init seed 2 D10
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
