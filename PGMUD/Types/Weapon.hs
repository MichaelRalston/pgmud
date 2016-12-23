module PGMUD.Types.Weapon 
    ( Weapon(..)
    ) where

import PGMUD.Types.Adjective
    
data Weapon = Weapon [Adjective] deriving (Show)
