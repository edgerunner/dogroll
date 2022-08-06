module Evergreen.V3.Die exposing (..)

import Evergreen.V3.Die.Size


type Rolled
    = Rolled Rolled


type Die x
    = HeldDie Evergreen.V3.Die.Size.Size
    | RolledDie Evergreen.V3.Die.Size.Size Int


type Held
    = Held Held