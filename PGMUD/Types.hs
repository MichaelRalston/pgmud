module PGMUD.Types
    ( Weapon
    , module PGMUD.Types.Stats
    , Adjective
    , WeaponClass
    , ItemLevel
    , Effect
    , ElementalAffinities (..)
    , Skill
    , APAmount (..)
    ) where

import PGMUD.Types.Weapon (Weapon)
import PGMUD.Types.Stats 
import PGMUD.Types.Adjective (Adjective)
import PGMUD.Types.Effect (Effect)
import PGMUD.Types.Skill (Skill)
import PGMUD.Types.Gear (WeaponClass, ItemLevel)

data ElementalAffinities = ElementalAffinities
data APAmount = APAmount Int