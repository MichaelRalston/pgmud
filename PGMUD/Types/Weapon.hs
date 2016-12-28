{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module PGMUD.Types.Weapon 
    ( Weapon(..)
    ) where

import PGMUD.Types.Adjective
import PGMUD.Types.GeneralClasses
    
data Weapon = Weapon { weaponAdjectives :: AdjectiveList } deriving (Show)

instance CanHasEIV e => HasEIV Weapon e where
    getEIV = mconcat . (map getEIV) . unwrapAdjectiveList . weaponAdjectives
