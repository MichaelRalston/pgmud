module PGMUD.Types.Weapon 
    ( Weapon(..)
    ) where

import PGMUD.Types.Adjective
import PGMUD.Types.Elements
import PGMUD.Types.Stats
    
data Weapon = Weapon { weaponAdjectives :: [Adjective] } deriving (Show)

instance HasElementalAffinities Weapon where
    elementalAffinities = mconcat . (map elementalAffinities) . weaponAdjectives
    
instance HasStatModifiers Weapon where
    baseStatModifiers = mconcat . (map baseStatModifiers) . weaponAdjectives
    derivedStatModifiers = mconcat . (map derivedStatModifiers) . weaponAdjectives
