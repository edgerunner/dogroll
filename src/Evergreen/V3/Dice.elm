module Evergreen.V3.Dice exposing (..)

import Evergreen.V3.Die


type Dice x
    = Dice (List (Evergreen.V3.Die.Die x))