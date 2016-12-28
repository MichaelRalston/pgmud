module PGMUD.Types
    ( Weapon
    , module PGMUD.Types.Stats
    , module PGMUD.Types.Elements
    , Adjective
    , WeaponClass
    , ItemLevel
    , Effect
    , Skill
    , APAmount (..)
    , AdjectiveType (..)
    , AdjectiveList
    ) where

import PGMUD.Types.Weapon (Weapon)
import PGMUD.Types.Stats 
import PGMUD.Types.Elements
import PGMUD.Types.Adjective (Adjective, AdjectiveType (..), AdjectiveList)
import PGMUD.Types.Effect (Effect)
import PGMUD.Types.Skill (Skill)
import PGMUD.Types.Gear (WeaponClass, ItemLevel)

data APAmount = APAmount Int