module PGMUD.Types.Weapon 
    ( Weapon(..)
    ) where

import PGMUD.Types.Adjective
import PGMUD.Types.Elements
    
data Weapon = Weapon { weaponAdjectives :: [Adjective] } deriving (Show)

instance HasElementalAffinities Weapon where
    elementalAffinities = mconcat . (map elementalAffinities) . weaponAdjectives